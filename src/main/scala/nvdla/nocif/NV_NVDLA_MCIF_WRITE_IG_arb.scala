package nvdla

import chisel3._
import chisel3.experimental._
import chisel3.util._


//Implementation overview of ping-pong register file.

class NV_NVDLA_MCIF_WRITE_IG_arb (implicit conf:nvdlaConfig) extends Module {
    val io = IO(new Bundle {
        //general clock
        val nvdla_core_clk = Input(Clock())

        //bpt2arb
        val bpt2arb_cmd_valid = Input(Vec(conf.WDMA_NUM, Bool()))
        val bpt2arb_cmd_ready = Output(Vec(conf.WDMA_NUM, Bool()))
        val bpt2arb_cmd_pd = Input(Vec(conf.WDMA_NUM, UInt((conf.NVDLA_DMA_WR_IG_PW).W)))

        val bpt2arb_dat_valid = Input(Vec(conf.WDMA_NUM, Bool()))
        val bpt2arb_dat_ready = Output(Vec(conf.WDMA_NUM, Bool()))
        val bpt2arb_dat_pd = Input(Vec(conf.WDMA_NUM, UInt((conf.NVDLA_DMA_WR_REQ-1).W)))

        //arb2spt
        val arb2spt_cmd_pd = Output(UInt((conf.NVDLA_DMA_WR_IG_PW).W))
        val arb2spt_cmd_valid = Output(Bool())
        val arb2spt_cmd_ready = Input(Bool())

        val arb2spt_dat_pd = Output(UInt((conf.NVDLA_DMA_WR_REQ-1).W))
        val arb2spt_dat_valid = Output(Bool())
        val arb2spt_dat_ready = Input(Bool())

        val pwrbus_ram_pd = Input(UInt(32.W))
        val reg2dp_wr_weight = Input(Vec(conf.WDMA_NUM, UInt(8.W)))
        })
    val arb_cmd_pd = RegInit("b0".asUInt(conf.NVDLA_DMA_WR_IG_PW.W))
    val arb_dat_pd = RegInit("b0".asUInt(conf.NVDLA_DMA_WR_REQ.W))
    val sticky = RegInit(false.B)

    val any_arb_gnt = Wire(Bool())

    val stick_gnts = RegInit("b0".asUInt(conf.WDMA_NUM.W))

    val all_gnts = Wire(UInt(conf.WDMA_NUM.W))
    val arb_gnts = Wire(Vec(conf.WDMA_NUM, Bool()))
    val arb_reqs = Wire(UInt(conf.WDMA_NUM.W))

    val gnt_busy = Wire(Bool())
    val spt_is_busy = Wire(Bool())
    val is_last_beat = Wire(Bool())
    val gnt_count = RegInit("b0".asUInt(3.W))
    val arb_cmd_size = RegInit("b0".asUInt(3.W))

    val src_cmd_beats    = Wire(Vec(conf.WDMA_NUM,UInt(3.W)))
    val src_cmd_camp_vld = Wire(Vec(conf.WDMA_NUM,Bool()))
    val src_cmd_pd       = Wire(Vec(conf.WDMA_NUM,UInt(conf.NVDLA_DMA_WR_IG_PW.W)))
    val src_cmd_rdy      = Wire(Vec(conf.WDMA_NUM,Bool()))
    val src_cmd_size     = Wire(Vec(conf.WDMA_NUM,UInt(3.W)))
    val src_cmd_vld      = Wire(Vec(conf.WDMA_NUM,Bool()))
    val src_dat_pd       = Wire(Vec(conf.WDMA_NUM,UInt((conf.NVDLA_DMA_WR_REQ-1).W)))
    val src_dat_rdy      = Wire(Vec(conf.WDMA_NUM,Bool()))
    val src_dat_vld      = Wire(Vec(conf.WDMA_NUM,Bool()))
    val dfifo_wr_count   = Wire(Vec(conf.WDMA_NUM,UInt(3.W)))

    val src_cmd_vlds    = VecInit((0 to conf.WDMA_NUM-1) map { i => src_cmd_camp_vld(i)}).asUInt


    val src_dat_rdy_sum  = Wire(Bool())
    val src_dat_vld_sum  = Wire(Bool())
    val src_dat_vlds    = VecInit((0 to conf.WDMA_NUM-1) map { i => src_dat_vld(i)}).asUInt
    val src_dat_gnts    = VecInit((0 to conf.WDMA_NUM-1) map { i => all_gnts(i)&src_dat_vlds(i)}).asUInt
    //val src_dat_rdys    = Wire(Bool())
    //val wt = Wire(Vec(5,UInt(8.W)))

    val pipe    = Array.fill(conf.WDMA_NUM){ Module(new NV_NVDLA_BC_pipe(conf.NVDLA_DMA_WR_IG_PW))}
    val u_dfifo = Array.fill(conf.WDMA_NUM){
                                 Module(new NV_NVDLA_fifo(depth = 8,
                                    width = 256, ram_type = 2,distant_wr_req = false,io_wr_empty = false,
                                    io_wr_idle = false,io_wr_count = true,io_rd_idle = false))
                                }
    is_last_beat := (gnt_count === arb_cmd_size);
    for(index <- 0 until conf.WDMA_NUM) {
        pipe(index).io.clk  := io.nvdla_core_clk

        src_cmd_pd(index) := pipe(index).io.dout
        src_cmd_vld(index) := pipe(index).io.vo
        pipe(index).io.ri   := src_cmd_rdy(index)

        pipe(index).io.di   := io.bpt2arb_cmd_pd(index)
        pipe(index).io.vi   := io.bpt2arb_cmd_valid(index)
        io.bpt2arb_cmd_ready(index)  := pipe(index).io.ro

        u_dfifo(index).io.clk := io.nvdla_core_clk

        dfifo_wr_count(index) := u_dfifo(index).io.wr_count.get
        io.bpt2arb_dat_ready(index) := u_dfifo(index).io.wr_prdy
        u_dfifo(index).io.wr_pvld := io.bpt2arb_dat_valid(index)

        u_dfifo(index).io.wr_pd   := io.bpt2arb_dat_pd(index)
        u_dfifo(index).io.rd_prdy := src_dat_rdy(index)
        src_dat_vld(index)  := u_dfifo(index).io.rd_pvld
        src_dat_pd(index)   := u_dfifo(index).io.rd_pd

        u_dfifo(index).io.pwrbus_ram_pd := io.pwrbus_ram_pd

        src_cmd_size(index) := Fill(3,src_cmd_vld(index)) & src_cmd_pd(index)(conf.NVDLA_MEM_ADDRESS_WIDTH+7,conf.NVDLA_MEM_ADDRESS_WIDTH+5)
        src_cmd_rdy(index)  := is_last_beat && (src_dat_rdy_sum === true.B) &&(src_dat_gnts(index) === true.B)
        src_dat_rdy(index)  := src_dat_rdy_sum === true.B && all_gnts(index) === true.B
        src_cmd_beats(index):= src_cmd_size(index)
        src_cmd_camp_vld(index) := src_cmd_vld(index) & (dfifo_wr_count(index) > src_cmd_beats(index))
    }
    arb_reqs := src_cmd_vlds

    val u_write_ig_arb = Module(new write_ig_arb(conf.WDMA_NUM,8))
    u_write_ig_arb.io.gnt_busy  := gnt_busy
    u_write_ig_arb.io.clk   := io.nvdla_core_clk

    for(index <- 0 until conf.WDMA_NUM) {
        u_write_ig_arb.io.req(index) := arb_reqs(index)
        u_write_ig_arb.io.wt(index) := io.reg2dp_wr_weight(index)
        arb_gnts(index) := u_write_ig_arb.io.gnt(index)
    }
    any_arb_gnt :=  arb_gnts.asUInt.orR()
    all_gnts := Mux(sticky,stick_gnts,arb_gnts.asUInt)
    gnt_busy := sticky || spt_is_busy

    src_dat_vld_sum := src_dat_gnts.asUInt().orR

    withClock(io.nvdla_core_clk){
        when(any_arb_gnt) {
            stick_gnts := arb_gnts.asUInt
        }

        when(any_arb_gnt) {
            when(src_dat_vld_sum && src_dat_rdy_sum && is_last_beat) {
                sticky := false.B
            }.otherwise {
                sticky := true.B
            }
        }.elsewhen(src_dat_vld_sum && src_dat_rdy_sum && is_last_beat){
                sticky := false.B
        }

        when(src_dat_vld_sum && src_dat_rdy_sum) {
            when(is_last_beat) {
                gnt_count := 0.U
            }.otherwise {
                gnt_count := gnt_count  + 1.U
            }
        }

        arb_cmd_pd := MuxCase(0.U,(0 until conf.WDMA_NUM) map { i => (all_gnts(i) === true.B) -> src_cmd_pd(i)})
        arb_cmd_size := arb_cmd_pd(conf.NVDLA_MEM_ADDRESS_WIDTH+7,conf.NVDLA_MEM_ADDRESS_WIDTH+5)

        arb_dat_pd := MuxCase(0.U,(0 until conf.WDMA_NUM) map { i => (all_gnts(i) === true.B) -> src_dat_pd(i)})

        io.arb2spt_cmd_pd := arb_cmd_pd
        io.arb2spt_dat_pd := arb_dat_pd
        io.arb2spt_cmd_valid := any_arb_gnt
        io.arb2spt_dat_valid := src_dat_vld_sum
        src_dat_rdy_sum := io.arb2spt_dat_ready
        spt_is_busy := !(io.arb2spt_cmd_ready & io.arb2spt_dat_ready);   //fixme
    }
}

object NV_NVDLA_MCIF_WRITE_IG_arbDriver extends App {
    implicit val conf: nvdlaConfig = new nvdlaConfig
    chisel3.Driver.execute(args, () => new NV_NVDLA_MCIF_WRITE_IG_arb())
}