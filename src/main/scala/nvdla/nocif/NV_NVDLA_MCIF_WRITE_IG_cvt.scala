package nvdla

import chisel3._
import chisel3.experimental._
import chisel3.util._

class NV_NVDLA_MCIF_WRITE_IG_cvt(implicit conf:nvdlaConfig) extends Module {
    val io = IO(new Bundle {
        //general clock
        val nvdla_core_clk = Input(Clock())

        //spt2cvt
        val spt2cvt_cmd_valid = Input(Bool())
        val spt2cvt_cmd_ready = Output(Bool())
        val spt2cvt_cmd_pd = Input(UInt((conf.NVDLA_DMA_WR_IG_PW).W))

        val spt2cvt_dat_valid = Input(Bool())
        val spt2cvt_dat_ready = Output(Bool())
        val spt2cvt_dat_pd = Input(UInt((conf.NVDLA_PRIMARY_MEMIF_WIDTH+conf.NVDLA_DMA_MASK_BIT).W))

        //cq_wr
        val cq_wr_pvld = Output(Bool())
        val cq_wr_prdy = Input(Bool())
        val cq_wr_pd = Output(UInt(3.W))
        val cq_wr_thread_id = Output(UInt(3.W))

        //mcif2noc
        val mcif2noc_axi_aw_awvalid = Output(Bool())
        val mcif2noc_axi_aw_awready = Input(Bool())
        val mcif2noc_axi_aw_awaddr = Output(UInt((conf.NVDLA_MEM_ADDRESS_WIDTH).W))
        val mcif2noc_axi_aw_awid = Output(UInt(8.W))
        val mcif2noc_axi_aw_awlen = Output(UInt(4.W))

        val mcif2noc_axi_w_wvalid = Output(Bool())
        val mcif2noc_axi_w_wready = Input(Bool())
        val mcif2noc_axi_w_wdata = Output(UInt((conf.NVDLA_PRIMARY_MEMIF_WIDTH).W))
        val mcif2noc_axi_w_wstrb = Output(UInt((conf.NVDLA_PRIMARY_MEMIF_STRB).W))
        val mcif2noc_axi_w_wlast = Output(Bool())

        //eg2ig
        val eg2ig_axi_len = Input(UInt(2.W))
        val eg2ig_axi_vld = Input(Bool())
        
        val reg2dp_wr_os_cnt = Input(UInt(8.W))
        })

        val eg2ig_axi_len_d = RegInit("b0".asUInt(2.W))
        val eg2ig_axi_vld_d = RegInit(false.B)
        val os_adv = RegInit(false.B)
        val os_cnt = RegInit("b0".asUInt(9.W))
        val os_cnt_cur = RegInit("b0".asUInt(9.W))
        val os_cnt_ext = RegInit("b0".asUInt(11.W))
        val os_cnt_mod = RegInit("b0".asUInt(11.W))
        val os_cnt_new = RegInit("b0".asUInt(11.W))
        val os_cnt_nxt = RegInit("b0".asUInt(11.W))
        val all_downs_rdy = Wire(Bool())
        val axi_both_rdy = Wire(Bool())
        val axi_addr = Wire(UInt(conf.NVDLA_MEM_ADDRESS_WIDTH.W))
        val axi_axid = Wire(UInt(4.W))
        val axi_aw_pd = Wire(UInt((conf.NVDLA_MEM_ADDRESS_WIDTH+6).W))
        val axi_cmd_pd = Wire(UInt((conf.NVDLA_MEM_ADDRESS_WIDTH+6).W))
        val axi_cmd_rdy = Wire(Bool())
        val axi_cmd_vld = Wire(Bool())
        val axi_dat_pd = Wire(UInt((conf.NVDLA_PRIMARY_MEMIF_WIDTH+conf.NVDLA_PRIMARY_MEMIF_STRB+1).W))
        val axi_dat_rdy = Wire(Bool())
        val axi_dat_vld = Wire(Bool())
        val axi_data = Wire(UInt(conf.NVDLA_PRIMARY_MEMIF_WIDTH.W))
        val axi_last = Wire(Bool())
        val axi_len = Wire(UInt(2.W))
        val axi_strb = Wire(UInt(conf.NVDLA_PRIMARY_MEMIF_STRB.W))
        val axi_w_pd = Wire((UInt((conf.NVDLA_PRIMARY_MEMIF_WIDTH+conf.NVDLA_PRIMARY_MEMIF_STRB+1).W)))
        val cfg_wr_os_cnt = Wire(UInt(8.W))
        val cmd_addr = Wire(UInt(conf.NVDLA_MEM_ADDRESS_WIDTH.W))
        val cmd_axid = Wire(UInt(4.W))
        val cmd_ftran = Wire(Bool())
        val cmd_inc = Wire(Bool())
        val cmd_ltran = Wire(Bool())
        val cmd_odd = Wire(Bool())
        val cmd_pd = Wire(UInt(conf.NVDLA_DMA_WR_IG_PW.W))
        val cmd_rdy = Wire(Bool())
        val cmd_require_ack = Wire(Bool())
        val cmd_size = Wire(UInt(3.W))
        val cmd_swizzle = Wire(Bool())
        //val cmd_user_size = Wire(UInt(3.W))
        val cmd_vld = Wire(Bool())
        val cmd_vld_pd = Wire(UInt(conf.NVDLA_DMA_WR_IG_PW.W))
        val cq_wr_len = Wire(UInt(2.W))
        val cq_wr_require_ack = Wire(Bool())
        val dat_data = Wire(UInt(conf.NVDLA_PRIMARY_MEMIF_WIDTH.W))
        val dat_mask = Wire(UInt(conf.NVDLA_DMA_MASK_BIT.W))
        val dat_pd = Wire(UInt((conf.NVDLA_PRIMARY_MEMIF_WIDTH+conf.NVDLA_PRIMARY_MEMIF_STRB+1).W))
        val dat_rdy = Wire(Bool())
        val dat_vld = Wire(Bool())

        val mon_thread_id_c = Wire(Bool())
        val is_first_beat = Wire(Bool())
        val is_single_beat = Wire(Bool())
        val is_last_beat = Wire(Bool())
        val beat_count = RegInit("b0".asUInt(2.W))
        val is_first_cmd_dat_vld = Wire(Bool())
        val opipe_axi_addr = Wire(UInt(conf.NVDLA_MEM_ADDRESS_WIDTH.W))
        val opipe_axi_axid = Wire(UInt(4.W))
        val opipe_axi_data = Wire(UInt(conf.NVDLA_PRIMARY_MEMIF_WIDTH.W))
        val opipe_axi_last = Wire(Bool())
        val opipe_axi_len = Wire(UInt(2.W))
        val opipe_axi_strb = Wire(UInt(conf.NVDLA_PRIMARY_MEMIF_STRB.W))
        val os_cmd_vld = Wire(Bool())
        val os_cnt_add = Wire(UInt(3.W))
        val os_cnt_add_en = Wire(Bool())
        val os_cnt_cen = Wire(Bool())
        val os_cnt_full = Wire(Bool())
        val os_cnt_sub = Wire(UInt(3.W))
        val os_cnt_sub_en = Wire(Bool())
        val os_inp_add_nxt = Wire(UInt(3.W))
        val os_inp_nxt = Wire(UInt(10.W))
        val os_inp_sub_nxt = Wire(UInt(2.W))
        val wr_os_cnt_ext = Wire(UInt(9.W))

        //pipe_p1
        val pipe_p1 = Module(new NV_NVDLA_BC_pipe(conf.NVDLA_DMA_WR_IG_PW))
        pipe_p1.io.clk := io.nvdla_core_clk

        pipe_p1.io.vi := io.spt2cvt_cmd_valid
        io.spt2cvt_cmd_ready := pipe_p1.io.ro
        pipe_p1.io.di := io.spt2cvt_cmd_pd

        cmd_pd  := pipe_p1.io.dout
        cmd_vld := pipe_p1.io.vo
        pipe_p1.io.ri := cmd_rdy

        //pipe_p2
        val pipe_p2 = Module(new NV_NVDLA_BC_pipe(conf.NVDLA_PRIMARY_MEMIF_WIDTH+conf.NVDLA_DMA_MASK_BIT))
        pipe_p2.io.clk  := io.nvdla_core_clk

        pipe_p2.io.vi   := io.spt2cvt_dat_valid
        io.spt2cvt_dat_ready := pipe_p2.io.ro
        pipe_p2.io.di   := io.spt2cvt_dat_pd

        dat_vld         := pipe_p2.io.vo
        pipe_p2.io.ri   := dat_rdy
        dat_pd          := pipe_p2.io.dout

        os_cmd_vld := cmd_vld & !os_cnt_full;
        //IG_cvt=== push into the cq on first beat of data
        dat_rdy := Mux(is_first_beat,os_cmd_vld & all_downs_rdy,axi_dat_rdy)
        //IG_cvt=== will release cmd on the acception of last beat of data
        cmd_rdy := is_first_beat & dat_vld & all_downs_rdy & !os_cnt_full;
        //IG_cvt===UNPACK after ipipe
        cmd_vld_pd := Fill(conf.NVDLA_DMA_WR_IG_PW, cmd_vld) & cmd_pd

        cmd_axid        := cmd_vld_pd(3,0)
        cmd_require_ack := cmd_vld_pd(4)
        cmd_addr        := cmd_vld_pd(conf.NVDLA_MEM_ADDRESS_WIDTH+4,5)
        cmd_size        := cmd_vld_pd(conf.NVDLA_MEM_ADDRESS_WIDTH+7,conf.NVDLA_MEM_ADDRESS_WIDTH+5)
        cmd_swizzle     := cmd_vld_pd(conf.NVDLA_MEM_ADDRESS_WIDTH+8)
        cmd_odd         := cmd_vld_pd(conf.NVDLA_MEM_ADDRESS_WIDTH+9)
        cmd_inc         := cmd_vld_pd(conf.NVDLA_MEM_ADDRESS_WIDTH+10)
        cmd_ltran       := cmd_vld_pd(conf.NVDLA_MEM_ADDRESS_WIDTH+11)
        cmd_ftran       := cmd_vld_pd(conf.NVDLA_MEM_ADDRESS_WIDTH+12)

        // PKT_UNPACK_WIRE( cvt_write_data , dat_ , dat_pd )
        dat_data :=  dat_pd(conf.NVDLA_PRIMARY_MEMIF_WIDTH-1,0)
        dat_mask :=  dat_pd(conf.NVDLA_PRIMARY_MEMIF_WIDTH+conf.NVDLA_DMA_MASK_BIT-1,conf.NVDLA_PRIMARY_MEMIF_WIDTH);

        axi_len := cmd_size(1,0)
        is_first_cmd_dat_vld := os_cmd_vld & dat_vld && is_first_beat

        is_first_beat := (beat_count === 0.U)
        is_single_beat := (axi_len === 0.U)
        is_last_beat  := (beat_count === 1.U || (beat_count === 0.U && is_single_beat))

        axi_axid := cmd_axid(3,0)
        axi_addr := cmd_addr
        axi_data := dat_data
        axi_last := is_last_beat
        axi_strb := Fill(conf.NVDLA_MEMORY_ATOMIC_SIZE,dat_mask)

        os_inp_add_nxt := Mux(cmd_vld,axi_len+1.U,0.U(3.W))

        os_inp_sub_nxt := Mux(eg2ig_axi_vld_d,eg2ig_axi_vld_d+1.U,0.U(3.W))
        os_inp_nxt     := os_cnt + os_inp_add_nxt - os_inp_sub_nxt

        // IG_cvt=== 256 outstanding trans
        os_cnt_add_en := axi_cmd_vld & axi_cmd_rdy
        os_cnt_sub_en := eg2ig_axi_vld_d
        os_cnt_cen    := os_cnt_add_en | os_cnt_sub_en
        os_cnt_add    := Mux(os_cnt_add_en,axi_len+1.U,0.U)
        os_cnt_sub    := Mux(os_cnt_sub_en,eg2ig_axi_len_d+1.U,0.U)
        cfg_wr_os_cnt := io.reg2dp_wr_os_cnt(7,0)
        wr_os_cnt_ext := Cat(false.B,cfg_wr_os_cnt)
        os_cnt_full   := os_inp_nxt>(wr_os_cnt_ext+1.U)

        os_adv  := os_cnt_add(2,0) =/= os_cnt_sub(2,0)

        os_cnt_ext := Cat(0.U,0.U,os_cnt_cur)
        os_cnt_mod := os_cnt_cur + os_cnt_add(2,0) - os_cnt_sub(2,0) // spyglass disable W164b
        os_cnt_new := Mux(os_adv,os_cnt_mod(10,0),os_cnt_ext(10,0))
        os_cnt_nxt := os_cnt_new(10,0)
        os_cnt     := os_cnt_cur(8,0)

        axi_cmd_vld      := is_first_cmd_dat_vld & io.cq_wr_prdy & axi_dat_rdy
        //pipe_p3
        val pipe_p3 = Module(new NV_NVDLA_BC_pipe(conf.NVDLA_MEM_ADDRESS_WIDTH+6))
        pipe_p3.io.clk  := io.nvdla_core_clk

        pipe_p3.io.vi   := axi_cmd_vld
        axi_cmd_rdy     := pipe_p3.io.ro
        pipe_p3.io.di   := axi_cmd_pd

        io.mcif2noc_axi_aw_awvalid := pipe_p3.io.vo
        pipe_p3.io.ri   := io.mcif2noc_axi_aw_awready
        axi_aw_pd := pipe_p3.io.dout

        axi_dat_vld := dat_vld & (~is_first_beat || (os_cmd_vld & io.cq_wr_prdy & axi_cmd_rdy))
        //pipe_p4
        val pipe_p4 = Module(new NV_NVDLA_BC_pipe(conf.NVDLA_PRIMARY_MEMIF_WIDTH+conf.NVDLA_PRIMARY_MEMIF_STRB+1))
        pipe_p4.io.clk  := io.nvdla_core_clk

        pipe_p4.io.vi   := axi_dat_vld
        axi_dat_rdy     := pipe_p4.io.ro
        pipe_p4.io.di   := axi_dat_pd

        io.mcif2noc_axi_w_wvalid   := pipe_p4.io.vo
        pipe_p4.io.ri   := io.mcif2noc_axi_w_wready
        axi_w_pd        := pipe_p4.io.dout


        axi_cmd_pd := Cat(axi_axid,axi_addr,axi_len)

        opipe_axi_axid := axi_aw_pd(conf.NVDLA_MEM_ADDRESS_WIDTH + 5,conf.NVDLA_MEM_ADDRESS_WIDTH+2)
        opipe_axi_addr := axi_aw_pd(conf.NVDLA_MEM_ADDRESS_WIDTH + 1 ,2)
        opipe_axi_len  := axi_aw_pd(1,0)
        axi_dat_pd := Cat(axi_data,axi_strb,axi_last)

        opipe_axi_data := axi_w_pd(conf.NVDLA_PRIMARY_MEMIF_WIDTH+conf.NVDLA_PRIMARY_MEMIF_STRB,conf.NVDLA_PRIMARY_MEMIF_STRB + 1)
        opipe_axi_strb := axi_w_pd(conf.NVDLA_PRIMARY_MEMIF_STRB,1)
        opipe_axi_last := axi_w_pd(0)

        // IG_cvt===AXI OUT ZERO EXT
        io.mcif2noc_axi_aw_awid     := opipe_axi_axid(3,0)
        io.mcif2noc_axi_aw_awaddr   := opipe_axi_addr
        io.mcif2noc_axi_aw_awlen    := Cat(Fill(2,0.U),opipe_axi_len)
        io.mcif2noc_axi_w_wlast     := opipe_axi_last
        io.mcif2noc_axi_w_wdata     := opipe_axi_data
        io.mcif2noc_axi_w_wstrb     := opipe_axi_strb

        axi_both_rdy  := axi_cmd_rdy & axi_dat_rdy
        all_downs_rdy := io.cq_wr_prdy & axi_both_rdy

        io.cq_wr_pvld := is_first_cmd_dat_vld & axi_both_rdy & !os_cnt_full
        cq_wr_require_ack := cmd_ltran & cmd_require_ack
        cq_wr_len := axi_len

        io.cq_wr_pd   := Cat(cq_wr_len(1,0),cq_wr_require_ack)

        io.cq_wr_thread_id := cmd_axid(3,1)
        mon_thread_id_c     := cmd_axid(0)

    withClock(io.nvdla_core_clk){
        when(is_first_cmd_dat_vld & all_downs_rdy) {
            beat_count := axi_len
        }.elsewhen((beat_count =/= 0.U) & dat_vld & axi_dat_rdy) {
            beat_count := beat_count - 1.U
        }

        eg2ig_axi_vld_d := io.eg2ig_axi_vld

        when(io.eg2ig_axi_vld === true.B) {
             eg2ig_axi_len_d := io.eg2ig_axi_len
        }
        when(os_cnt_cen) {
            os_cnt_cur := os_cnt_nxt(8,0)
        }
    }
}

object NV_NVDLA_MCIF_WRITE_IG_cvtDriver extends App {
    implicit val conf: nvdlaConfig = new nvdlaConfig
    chisel3.Driver.execute(args, () => new NV_NVDLA_MCIF_WRITE_IG_cvt())
}
