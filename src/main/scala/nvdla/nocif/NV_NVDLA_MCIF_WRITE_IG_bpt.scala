package nvdla

import chisel3._
import chisel3.experimental._
import chisel3.util._

//Implementation overview of ping-pong register file.

class NV_NVDLA_MCIF_WRITE_IG_bpt (implicit conf:nvdlaConfig) extends Module {
    val io = IO(new Bundle {
        //general clock
        val nvdla_core_clk = Input(Clock())
        val pwrbus_ram_pd = Input(UInt(32.W))

        val axid = Input(UInt(4.W))
        
        //dma2bpt
        val dma2bpt_req_valid = Input(Bool())
        val dma2bpt_req_ready = Output(Bool())
        val dma2bpt_req_pd = Input(UInt((conf.NVDLA_DMA_WR_REQ-1).W))

        //bpt2arb
        val bpt2arb_cmd_valid = Output(Bool())
        val bpt2arb_cmd_ready = Input(Bool())
        val bpt2arb_cmd_pd = Output(UInt((conf.NVDLA_DMA_WR_IG_PW).W))

        val bpt2arb_dat_valid = Output(Bool())
        val bpt2arb_dat_ready = Input(Bool())
        val bpt2arb_dat_pd = Output(UInt((conf.NVDLA_DMA_WR_REQ-1).W))
    })
    val cmd_en = RegInit(false.B)
    val dat_en = RegInit(false.B)
    val ipipe_pd = Wire(UInt(conf.NVDLA_DMA_WR_REQ.W))
    val ipipe_pd_p = Wire(UInt(conf.NVDLA_DMA_WR_REQ.W))

    val ipipe_rdy = Wire(Bool())
    val ipipe_rdy_p = Wire(Bool())
    val ipipe_vld = Wire(Bool())
    val ipipe_vld_p = Wire(Bool())

    val ipipe_cmd_pd = Wire(UInt(conf.NVDLA_DMA_WR_CMD.W))
    val ipipe_cmd_vld = Wire(Bool())
    val ipipe_cmd_rdy = Wire(Bool())

    val in_cmd_pd = Wire(UInt(conf.NVDLA_DMA_WR_CMD.W))
    val in_cmd_vld_pd = Wire(UInt(conf.NVDLA_DMA_WR_CMD.W))
    val in_cmd_vld = Wire(Bool())
    val in_cmd_rdy = Wire(Bool())

    val in_cmd_addr = Wire(UInt(conf.NVDLA_MEM_ADDRESS_WIDTH.W))
    val in_cmd_size = Wire(UInt(conf.NVDLA_DMA_WR_SIZE.W))
    val in_cmd_require_ack = Wire(Bool())

    val dfifo_wr_mask = Wire(UInt(conf.NVDLA_DMA_MASK_BIT.W))
    val dfifo_wr_data = Wire(UInt(conf.NVDLA_MEMIF_WIDTH.W))
    val dfifo_wr_prdy = Wire(Bool())
    val dfifo_wr_pvld = Wire(Bool())

    val dfifo_rd_data = Wire(UInt(conf.NVDLA_MEMIF_WIDTH.W))
    val dfifo_rd_prdy = Wire(Bool())
    val dfifo_rd_pvld = Wire(Bool())

    val out_addr = RegInit("b0".asUInt(conf.NVDLA_MEM_ADDRESS_WIDTH.W))
    val out_size = Wire(UInt(3.W))
    val out_size_tmp = RegInit("b0".asUInt(3.W))

    val req_count = RegInit("b0".asUInt(conf.NVDLA_DMA_WR_SIZE.W))
    val req_num = RegInit("b0".asUInt(conf.NVDLA_DMA_WR_SIZE.W))

    val beat_count = RegInit("b0".asUInt(3.W))
    val beat_size = Wire(UInt(3.W))
    val is_last_beat = Wire(Bool())

    val bpt2arb_cmd_accept = Wire(Bool())
    val bpt2arb_dat_accept = Wire(Bool())

    if(conf.NVDLA_PRIMARY_MEMIF_WIDTH > conf.NVDLA_MEMORY_ATOMIC_WIDTH) {
        val mon_out_beats_c = Wire(Bool())
        val all_idle = Wire(UInt(2.W))
        val bpt_idle_NC  = Wire(Bool())
        val dfifo_rd_pd = Wire(Vec(2,UInt(conf.NVDLA_MEMORY_ATOMIC_WIDTH.W)));
        val dfifo_rd_prdy = Wire(Vec(2,Bool()))
        val dfifo_rd_pvld = Wire(Vec(2,Bool()))

        val dfifo_wr_idle = Wire(Vec(2,Bool()))
        val dfifo_wr_pd = Wire(Vec(2,UInt(conf.NVDLA_MEMORY_ATOMIC_WIDTH.W)));
        val dfifo_wr_prdy = Wire(Vec(2,Bool()))
        val dfifo_wr_pvld = Wire(Vec(2,Bool()))

        val mon_dfifo_wr_pd = Wire(Vec(2,Bool()))
        val in_dat_vld  = RegInit(false.B)
        val in_dat_cnt  = RegInit("b0".asUInt(conf.NVDLA_DMA_WR_SIZE.W))
        val in_dat1_dis = RegInit(false.B)
        val in_dat0_dis = Wire(Bool())

        val in_dat_data = Wire(Vec(2,UInt(conf.NVDLA_MEMORY_ATOMIC_WIDTH.W)))
        val in_dat_en = Wire(Vec(2,Bool()))
        val in_dat_mask = Wire(Vec(2,Bool()))
        val in_dat_pvld = Wire(Vec(2,Bool()))
        val in_dat_beats = Wire(UInt(conf.NVDLA_DMA_WR_SIZE.W))
        val in_dat_first = Wire(Bool())
        val in_dat_last = Wire(Bool())
        val swizzle_dat_data = Wire(Vec(2,UInt(conf.NVDLA_MEMORY_ATOMIC_WIDTH.W)))
        val swizzle_dat_mask = Wire(Vec(2,UInt(conf.NVDLA_MEMORY_ATOMIC_WIDTH.W)))
    }

    val ftran_size = Wire(UInt(3.W))
    val ltran_size = Wire(UInt(3.W))

    val mtran_num = Wire(UInt(conf.NVDLA_DMA_WR_SIZE.W))

    val  is_ftran = Wire(Bool())
    val  is_ltran = Wire(Bool())
    val  is_mtran = Wire(Bool())

    val out_cmd_addr = Wire(UInt(conf.NVDLA_MEM_ADDRESS_WIDTH.W))
    val out_cmd_axid = Wire(UInt(4.W))
    val out_cmd_ftran = Wire(Bool())
    val out_cmd_inc = Wire(Bool())
    val out_cmd_ltran = Wire(Bool())
    val out_cmd_odd = Wire(Bool())
    val out_cmd_require_ack = Wire(Bool())

    val out_cmd_size = Wire(UInt(3.W))
    val out_cmd_swizzle = Wire(Bool())

    val out_cmd_vld = Wire(Bool())
    val out_dat_data = Wire(UInt(conf.NVDLA_MEMIF_WIDTH.W))
    val out_dat_mask = Wire(UInt(conf.NVDLA_DMA_MASK_BIT.W))
    val out_dat_vld = Wire(Bool())

    val pipe_p1 = Module{new NV_NVDLA_IS_pipe(conf.NVDLA_DMA_WR_REQ)}
    pipe_p1.io.clk := io.nvdla_core_clk
    
    pipe_p1.io.di  := io.dma2bpt_req_pd
    io.dma2bpt_req_ready := pipe_p1.io.ro
    pipe_p1.io.vi := io.dma2bpt_req_valid

    ipipe_pd_p := pipe_p1.io.dout
    ipipe_vld_p := pipe_p1.io.vo
    pipe_p1.io.ri := ipipe_rdy_p

    val pipe_p2 = Module{new NV_NVDLA_IS_pipe(conf.NVDLA_DMA_WR_REQ)}
    pipe_p2.io.clk := io.nvdla_core_clk

    pipe_p2.io.di := ipipe_pd_p
    ipipe_rdy_p := pipe_p2.io.ro
    pipe_p2.io.vi := ipipe_vld_p

    ipipe_pd := pipe_p2.io.dout
    ipipe_vld := pipe_p2.io.vo
    pipe_p2.io.ri := ipipe_rdy

    ipipe_cmd_vld := ipipe_vld && (ipipe_pd(conf.NVDLA_DMA_WR_REQ-1) === 0.U)
    dfifo_wr_pvld := ipipe_vld && (ipipe_pd(conf.NVDLA_DMA_WR_REQ-1) === 1.U);

    dfifo_wr_data := ipipe_pd
    dfifo_wr_mask := ipipe_pd(conf.NVDLA_MEMIF_WIDTH,conf.NVDLA_MEMIF_WIDTH+conf.NVDLA_DMA_MASK_BIT-1)
    ipipe_rdy := (ipipe_cmd_vld & ipipe_cmd_rdy) || (dfifo_wr_pvld & dfifo_wr_prdy)
    ipipe_cmd_pd := ipipe_pd

    val pipe_p3 = Module{new NV_NVDLA_BC_pipe(conf.NVDLA_DMA_WR_CMD)}
    pipe_p3.io.clk := io.nvdla_core_clk

    pipe_p3.io.di := ipipe_cmd_pd
    pipe_p3.io.vi := ipipe_cmd_vld
    ipipe_cmd_rdy := pipe_p3.io.ro

    in_cmd_pd := pipe_p3.io.dout
    in_cmd_vld := pipe_p3.io.vo
    pipe_p3.io.ri := in_cmd_rdy

    in_cmd_rdy := is_ltran & is_last_beat & bpt2arb_dat_accept
    in_cmd_vld_pd := Fill(conf.NVDLA_DMA_WR_CMD,in_cmd_vld) & in_cmd_pd
    in_cmd_addr := in_cmd_vld_pd(conf.NVDLA_MEM_ADDRESS_WIDTH-1,0)
    in_cmd_size := in_cmd_vld_pd(conf.NVDLA_DMA_WR_CMD-2,conf.NVDLA_MEM_ADDRESS_WIDTH)
    in_cmd_require_ack := in_cmd_vld_pd(conf.NVDLA_DMA_WR_CMD-1,0)

    val u_dfifo = Module{new NV_NVDLA_IS_pipe(conf.NVDLA_MEMIF_WIDTH)}
    u_dfifo.io.clk := io.nvdla_core_clk

    dfifo_rd_data     := u_dfifo.io.dout
    dfifo_rd_pvld   := u_dfifo.io.vo
    u_dfifo.io.ri   := dfifo_rd_prdy

    u_dfifo.io.di   := dfifo_wr_data
    u_dfifo.io.vi   := dfifo_wr_pvld
    dfifo_wr_prdy   := u_dfifo.io.ro

    if(conf.NVDLA_MCIF_BURST_SIZE > 1) {
        val stt_offset = Wire(UInt(conf.NVDLA_MCIF_BURST_SIZE_LOG2.W))
        val size_offset = Wire(UInt(conf.NVDLA_MCIF_BURST_SIZE_LOG2.W))
        val end_offset = Wire(UInt(conf.NVDLA_MCIF_BURST_SIZE_LOG2.W))
        val ftran_size_tmp = Wire(UInt(conf.NVDLA_MCIF_BURST_SIZE_LOG2.W))
        val ltran_size_tmp = Wire(UInt(conf.NVDLA_MCIF_BURST_SIZE_LOG2.W))
        val  is_single_tran = Wire(Bool())
        val  mon_end_offset_c = Wire(Bool())

        stt_offset  := in_cmd_addr(conf.NVDLA_MEMORY_ATOMIC_LOG2 + conf.NVDLA_MCIF_BURST_SIZE_LOG2-1,conf.NVDLA_MEMORY_ATOMIC_LOG2)
        size_offset := in_cmd_size(conf.NVDLA_MCIF_BURST_SIZE_LOG2-1,0)
        val temp_result = stt_offset +& size_offset
        mon_end_offset_c := temp_result(conf.NVDLA_MCIF_BURST_SIZE_LOG2)
        end_offset       := temp_result(conf.NVDLA_MCIF_BURST_SIZE_LOG2-1, 0)


        // calculate how many trans to be split
        is_single_tran := (stt_offset + in_cmd_size) < conf.NVDLA_MCIF_BURST_SIZE.U
        ftran_size_tmp := Mux(is_single_tran,size_offset,(conf.NVDLA_MCIF_BURST_SIZE-1).U - stt_offset)
        ltran_size_tmp := Mux(is_single_tran,size_offset,end_offset)
        ftran_size := Cat (Fill(3-conf.NVDLA_MCIF_BURST_SIZE_LOG2, 0.U), ftran_size_tmp)
        ltran_size := Cat (Fill(3-conf.NVDLA_MCIF_BURST_SIZE_LOG2, 0.U), ltran_size_tmp)
        mtran_num := in_cmd_size - ftran_size - ltran_size - 1.U

        out_size_tmp := 0.U
        when(is_ftran ){
            out_size_tmp := ftran_size
        } .elsewhen(is_mtran) {
            out_size_tmp := conf.NVDLA_MCIF_BURST_SIZE.U
        } .elsewhen(is_ltran) {
            out_size_tmp := ltran_size
        }
        out_size := out_size_tmp

        when(is_single_tran) {
            req_num := 0.U
        }.otherwise {
            req_num := 1.U + mtran_num(12,conf.NVDLA_MCIF_BURST_SIZE_LOG2)
        }

    } else {
        ftran_size := 0.U(3.W)
        ltran_size := 0.U(3.W)
        mtran_num := in_cmd_size - 1.U

        out_size := 0.U(3.W)
        req_num := in_cmd_size
    }

    dfifo_rd_prdy := dat_en & io.bpt2arb_dat_ready

    out_dat_vld  := dat_en & dfifo_rd_pvld
    out_dat_data := dfifo_rd_data
    out_dat_mask := dfifo_rd_pvld
    beat_size   := out_size

    is_last_beat := (beat_count === beat_size)
    withClock(io.nvdla_core_clk){
        when(bpt2arb_cmd_accept) {
            cmd_en := false.B
            dat_en := true.B
        }.elsewhen(bpt2arb_dat_accept & is_last_beat ){
            cmd_en := true.B
            dat_en := false.B
        }
        when(bpt2arb_dat_accept) {
            when(is_last_beat) {
                beat_count := 0.U
            }.otherwise{
                beat_count := beat_count + 1.U
            }
        }

        when(bpt2arb_cmd_accept) {
            when(is_ftran) {
                out_addr := in_cmd_addr + ((ftran_size + 1.U) << 5)
            }.otherwise {
                out_addr := out_addr + (1.U << 5)
            }
        }

        when(bpt2arb_dat_accept & is_last_beat) {
            when(is_ltran) {
                req_count := 0.U
            }.otherwise {
                req_count := req_count + 1.U
            }
        }
    }

    is_ftran := (req_count === 0.U)
    is_mtran := (req_count > 0.U && req_count < req_num)
    is_ltran := (req_count === req_num)

    out_cmd_vld := cmd_en & in_cmd_vld
    out_cmd_addr := Mux(is_ftran,in_cmd_addr,out_addr)
    out_cmd_size := out_size
    out_cmd_inc := 0.U
    out_cmd_swizzle := 0.U
    out_cmd_odd := 0.U
    out_cmd_ftran := is_ftran
    out_cmd_ltran := is_ltran
    out_cmd_axid  := io.axid
    out_cmd_require_ack := in_cmd_require_ack & is_ltran


    io.bpt2arb_cmd_pd := Cat(out_cmd_ftran,out_cmd_ltran,out_cmd_inc,out_cmd_odd,out_cmd_swizzle,out_cmd_size,out_cmd_addr,out_cmd_require_ack,out_cmd_axid)
    io.bpt2arb_dat_pd := Cat(out_dat_mask,out_dat_data)


    io.bpt2arb_cmd_valid := out_cmd_vld
    io.bpt2arb_dat_valid := out_dat_vld
    bpt2arb_cmd_accept  := io.bpt2arb_cmd_valid & io.bpt2arb_cmd_ready
    bpt2arb_dat_accept  := io.bpt2arb_dat_valid & io.bpt2arb_dat_ready
}

object NV_NVDLA_MCIF_WRITE_IG_bptDriver extends App {
    implicit val conf: nvdlaConfig = new nvdlaConfig
    chisel3.Driver.execute(args, () => new NV_NVDLA_MCIF_WRITE_IG_bpt())
}