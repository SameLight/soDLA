package nvdla

import chisel3._
import chisel3.experimental._
import chisel3.util._

//Implementation overview of ping-pong register file.

class NV_NVDLA_MCIF_WRITE_eg (implicit conf: nvdlaConfig) extends Module {
    val io = IO(new Bundle {
        //general clock
        val nvdla_core_clk = Input(Clock())      
       // val nvdla_core_rstn = Input(Bool())

        val mcif2sdp_wr_rsp_complete = Output(Bool())
        val mcif2pdp_wr_rsp_complete = if(conf.NVDLA_PDP_ENABLE) Some(Output(Bool())) else None
        val mcif2cdp_wr_rsp_complete = if(conf.NVDLA_CDP_ENABLE) Some(Output(Bool())) else None
        val mcif2bdma_wr_rsp_complete =if(conf.NVDLA_BDMA_ENABLE) Some(Output(Bool())) else None
        val mcif2rbk_wr_rsp_complete = if(conf.NVDLA_RUBIK_ENABLE) Some(Output(Bool())) else None

        //cq_rd
        val cq_rd_pvld = Input(Vec(conf.WDMA_MAX_NUM, Bool()))
        val cq_rd_prdy = Output(Vec(conf.WDMA_MAX_NUM, Bool()))
        val cq_rd_pd = Input(Vec(conf.WDMA_MAX_NUM, UInt(3.W)))

        //noc2mcif
        val noc2mcif_axi_b_bvalid = Input(Bool())
        val noc2mcif_axi_b_bready = Output(Bool())
        val noc2mcif_axi_b_bid = Input(UInt(8.W))

        //eg2ig
        val eg2ig_axi_len = Output(UInt(2.W))
        val eg2ig_axi_vld = Output(Bool())
    })
    val iflop_axi_axid = Wire(UInt(3.W))
    val iflop_axi_vld  = Wire(Bool())
    val iflop_axi_rdy  = Wire(Bool())
    val u_pipe = Module(new NV_NVDLA_IS_pipe(3))

    u_pipe.io.clk    := io.nvdla_core_clk
    iflop_axi_axid := u_pipe.io.dout
    iflop_axi_vld  := u_pipe.io.vo
    u_pipe.io.ri   := iflop_axi_rdy

    u_pipe.io.di := io.noc2mcif_axi_b_bid(2,0)
    u_pipe.io.vi := io.noc2mcif_axi_b_bvalid
    io.noc2mcif_axi_b_bready := u_pipe.io.ro

    val iflop_axi_rdy_vec   = Wire(Vec(conf.WDMA_MAX_NUM,Bool()))
    val iflop_axi_vld_vec   = Wire(Vec(conf.WDMA_MAX_NUM,Bool()))
    val cq_rd_len_vec       = Wire(Vec(conf.WDMA_MAX_NUM,UInt(2.W)))

    for(index <- 0 until conf.WDMA_MAX_NUM) {
        iflop_axi_rdy_vec(index) := io.cq_rd_pvld(index) &&(iflop_axi_axid === index.U)
        iflop_axi_vld_vec(index) := iflop_axi_vld &&(iflop_axi_axid === index.U)
        cq_rd_len_vec(index)     := io.cq_rd_pd(index)(2,1)
        io.cq_rd_prdy(index)     := iflop_axi_vld_vec(index)
        iflop_axi_rdy :=  iflop_axi_rdy_vec(index)
    }
    io.eg2ig_axi_len := MuxCase(
        0.U,
        (0 until conf.WDMA_MAX_NUM) map { i => (iflop_axi_vld_vec(i) === true.B) -> cq_rd_len_vec(i)}
    )
    //sdp

    val sdp_cq_rd_pvld = io.cq_rd_pvld(conf.tieoff_axid_sdp)
    val sdp_cq_rd_ack = io.cq_rd_pd(conf.tieoff_axid_sdp)
    val sdp_axi_vld = iflop_axi_vld && (iflop_axi_axid === conf.tieoff_axid_sdp.U)

    io.eg2ig_axi_vld := iflop_axi_vld & iflop_axi_rdy
    withClock(io.nvdla_core_clk){
        io.mcif2sdp_wr_rsp_complete := sdp_axi_vld && sdp_cq_rd_pvld && (sdp_cq_rd_ack =/= 0.U)

        if(conf.NVDLA_PDP_ENABLE) {
            //pdp
            val pdp_cq_rd_pvld = Wire(Bool())
            val pdp_cq_rd_ack  = Wire(Bool())
            val pdp_axi_vld    = Wire(UInt(3.W))

            pdp_cq_rd_pvld := io.cq_rd_pvld(conf.tieoff_axid_pdp)
            pdp_cq_rd_ack := io.cq_rd_pvld(conf.tieoff_axid_pdp)
            pdp_axi_vld := iflop_axi_vld && (iflop_axi_axid === conf.tieoff_axid_pdp.U)
            io.mcif2pdp_wr_rsp_complete.get  := pdp_cq_rd_pvld && pdp_cq_rd_ack && (pdp_axi_vld =/= 0.U)
        }
        if(conf.NVDLA_CDP_ENABLE) {
            val cdp_cq_rd_pvld = Wire(Bool())
            val cdp_cq_rd_ack  = Wire(Bool())
            val cdp_axi_vld    = Wire(UInt(3.W))
            cdp_cq_rd_pvld := io.cq_rd_pvld(conf.tieoff_axid_cdp)
            cdp_cq_rd_ack := io.cq_rd_pvld(conf.tieoff_axid_cdp)
            cdp_axi_vld := iflop_axi_vld && (iflop_axi_axid === conf.tieoff_axid_cdp.U)
            io.mcif2cdp_wr_rsp_complete.get  :=    cdp_cq_rd_pvld && cdp_cq_rd_ack && (cdp_axi_vld =/= 0.U)
        }
        if(conf.NVDLA_BDMA_ENABLE) {
            val bdma_cq_rd_pvld =Wire(Bool())
            val bdma_cq_rd_ack  =Wire(Bool())
            val bdma_axi_vld    =Wire(UInt(3.W))
            bdma_cq_rd_pvld := io.cq_rd_pvld(conf.tieoff_axid_bdma)
            bdma_cq_rd_ack := io.cq_rd_pvld(conf.tieoff_axid_bdma)
            bdma_axi_vld := iflop_axi_vld && (iflop_axi_axid === conf.tieoff_axid_bdma.U)
            io.mcif2bdma_wr_rsp_complete.get :=    bdma_cq_rd_pvld && bdma_cq_rd_ack && (bdma_axi_vld =/= 0.U)
        }
        if(conf.NVDLA_RUBIK_ENABLE) {
            val rbk_cq_rd_pvld = Wire(Bool())
            val rbk_cq_rd_ack  = Wire(Bool())
            val rbk_axi_vld    = Wire(UInt(3.W))
            rbk_cq_rd_pvld := io.cq_rd_pvld(conf.tieoff_axid_rbk)
            rbk_cq_rd_ack := io.cq_rd_pvld(conf.tieoff_axid_rbk)
            rbk_axi_vld := iflop_axi_vld && (iflop_axi_axid === conf.tieoff_axid_rbk.U)
            io.mcif2rbk_wr_rsp_complete.get  :=    rbk_cq_rd_pvld && rbk_cq_rd_ack && (rbk_axi_vld =/= 0.U)
        }
    }
}

object NV_NVDLA_MCIF_WRITE_egDriver extends App {
    implicit val conf: nvdlaConfig = new nvdlaConfig
    chisel3.Driver.execute(args, () => new NV_NVDLA_MCIF_WRITE_eg())
}