package nvdla

import chisel3._
import chisel3.experimental._
import chisel3.util._

//Implementation overview of ping-pong register file.

class NV_NVDLA_MCIF_write(implicit conf: nvdlaConfig) extends Module {
    val io = IO(new Bundle {
        //general clock
        val nvdla_core_clk = Input(Clock())
        //val nvdla_core_rstn = Input(Bool())
        val pwrbus_ram_pd = Input(UInt(32.W))
        val reg2dp_wr_os_cnt = Input(UInt(8.W))

        //weight
        val reg2dp_wr_weight_sdp = Input(UInt(8.W))
        val reg2dp_wr_weight_bdma = if(conf.NVDLA_BDMA_ENABLE) Some(Input(UInt(8.W))) else None
        val reg2dp_wr_weight_rbk = if(conf.NVDLA_RUBIK_ENABLE) Some(Input(UInt(8.W))) else None
        val reg2dp_wr_weight_cdp = if(conf.NVDLA_CDP_ENABLE) Some(Input(UInt(8.W))) else None
        val reg2dp_wr_weight_pdp = if(conf.NVDLA_PDP_ENABLE) Some(Input(UInt(8.W))) else None

        //sdp
        val sdp2mcif_wr_req_pd = Input(UInt(conf.NVDLA_DMA_WR_REQ.W))
        val sdp2mcif_wr_req_valid = Input(Bool())
        val sdp2mcif_wr_req_ready = Output(Bool())
        val mcif2sdp_wr_rsp_complete = Output(Bool())

        //bdma
        val bdma2mcif_wr_req_pd          = if(conf.NVDLA_BDMA_ENABLE) Some(Input(UInt(conf.NVDLA_DMA_WR_REQ.W))) else None
        val bdma2mcif_wr_req_valid       = if(conf.NVDLA_BDMA_ENABLE) Some(Input(Bool())) else None
        val bdma2mcif_wr_req_ready       = if(conf.NVDLA_BDMA_ENABLE) Some(Output(Bool())) else None
        val mcif2bdma_wr_rsp_complete    = if(conf.NVDLA_BDMA_ENABLE) Some(Output(Bool())) else None


        //rbk
        val rbk2mcif_wr_req_pd          = if(conf.NVDLA_RUBIK_ENABLE) Some(Input(UInt(conf.NVDLA_DMA_WR_REQ.W))) else None
        val rbk2mcif_wr_req_valid       = if(conf.NVDLA_RUBIK_ENABLE) Some(Input(Bool())) else None
        val rbk2mcif_wr_req_ready       = if(conf.NVDLA_RUBIK_ENABLE) Some(Output(Bool())) else None
        val mcif2rbk_wr_rsp_complete    = if(conf.NVDLA_RUBIK_ENABLE) Some(Output(Bool())) else None


        //pdp
        val pdp2mcif_wr_req_pd          = if(conf.NVDLA_PDP_ENABLE)  Some(Input(UInt(conf.NVDLA_DMA_WR_REQ.W))) else None
        val pdp2mcif_wr_req_valid       = if(conf.NVDLA_PDP_ENABLE) Some(Input(Bool())) else None
        val pdp2mcif_wr_req_ready       = if(conf.NVDLA_PDP_ENABLE) Some(Output(Bool())) else None
        val mcif2pdp_wr_rsp_complete    = if(conf.NVDLA_PDP_ENABLE) Some(Output(Bool())) else None

        //cdp
        val cdp2mcif_wr_req_pd          = if(conf.NVDLA_CDP_ENABLE)  Some(Input(UInt(conf.NVDLA_DMA_WR_REQ.W))) else None
        val cdp2mcif_wr_req_valid       = if(conf.NVDLA_CDP_ENABLE) Some(Input(Bool())) else None
        val cdp2mcif_wr_req_ready       = if(conf.NVDLA_CDP_ENABLE) Some(Output(Bool())) else None
        val mcif2cdp_wr_rsp_complete    = if(conf.NVDLA_CDP_ENABLE) Some(Output(Bool())) else None

        //noc2mcif
        val noc2mcif_axi_b_bid = Input(UInt(8.W))
        val noc2mcif_axi_b_bvalid = Input(Bool())
        val noc2mcif_axi_b_bready = Output(Bool())
        val mcif2noc_axi_w_wlast = Output(Bool())
        val mcif2noc_axi_w_wstrb = Output(UInt(conf.NVDLA_PRIMARY_MEMIF_STRB.W))
        val mcif2noc_axi_w_wdata = Output(UInt(conf.NVDLA_PRIMARY_MEMIF_WIDTH.W))
        val mcif2noc_axi_w_wready = Input(Bool())
        val mcif2noc_axi_w_wvalid = Output(Bool())

        val mcif2noc_axi_aw_awaddr = Output(UInt(conf.NVDLA_MEM_ADDRESS_WIDTH.W))

        val mcif2noc_axi_aw_awlen = Output(UInt(4.W))
        val mcif2noc_axi_aw_awid = Output(UInt(4.W))
        val mcif2noc_axi_aw_awready = Input(Bool())
        val mcif2noc_axi_aw_awvalid = Output(Bool())
    })//end NV_NVDLA_MCIF_write IO

    /*NV_NVDLA_MCIF_WRITE_ig --- NV_NVDLA_MCIF_WRITE_eg*/
    val eg2ig_axi_vld = Wire(Bool())
    val eg2ig_axi_len = Wire(UInt(2.W))
    val cq_wr_thread_id = Wire(UInt(3.W))
    val cq_wr_pd = Wire(UInt(3.W))
    val cq_wr_prdy = Wire(Bool())
    val cq_wr_pvld = Wire(Bool())

    /*NV_NVDLA_MCIF_WRITE_cq --- NV_NVDLA_MCIF_WRITE_eg*/
    val cq_rd_pd    = Wire(Vec(conf.WDMA_MAX_NUM, UInt(3.W)))
    val cq_rd_pvld  = Wire(Vec(conf.WDMA_MAX_NUM, Bool()))
    val cq_rd_prdy  = Wire(Vec(conf.WDMA_MAX_NUM, Bool()))

    val u_ig = Module(new NV_NVDLA_MCIF_WRITE_ig)
    u_ig.io.nvdla_core_clk := io.nvdla_core_clk
    //u_ig.io.nvdla_core_rstn := io.nvdla_core_rstn
    u_ig.io.pwrbus_ram_pd := io.pwrbus_ram_pd
    u_ig.io.reg2dp_wr_os_cnt := io.reg2dp_wr_os_cnt

    //sdp
    u_ig.io.reg2dp_wr_weight_sdp := io.reg2dp_wr_weight_sdp
    u_ig.io.sdp2mcif_wr_req_pd := io.sdp2mcif_wr_req_pd
    io.sdp2mcif_wr_req_ready := u_ig.io.sdp2mcif_wr_req_ready
    u_ig.io.sdp2mcif_wr_req_valid := io.sdp2mcif_wr_req_valid

    //bdma
    if(conf.NVDLA_BDMA_ENABLE) {
        u_ig.io.reg2dp_wr_weight_bdma.get := io.reg2dp_wr_weight_bdma.get
        u_ig.io.bdma2mcif_wr_req_pd.get   := io.bdma2mcif_wr_req_pd.get
        io.bdma2mcif_wr_req_ready.get := u_ig.io.bdma2mcif_wr_req_ready.get
        u_ig.io.bdma2mcif_wr_req_valid.get := io.bdma2mcif_wr_req_valid.get
    }

    //cdp
    if(conf.NVDLA_CDP_ENABLE) {
        u_ig.io.reg2dp_wr_weight_cdp.get  := io.reg2dp_wr_weight_cdp.get
        u_ig.io.cdp2mcif_wr_req_pd.get := io.cdp2mcif_wr_req_pd.get
        io.cdp2mcif_wr_req_ready.get := u_ig.io.cdp2mcif_wr_req_ready.get
        u_ig.io.cdp2mcif_wr_req_valid.get := io.cdp2mcif_wr_req_valid.get
    }

    //rbk
    if(conf.NVDLA_RUBIK_ENABLE) {
        u_ig.io.reg2dp_wr_weight_rbk.get := io.reg2dp_wr_weight_rbk.get
        u_ig.io.rbk2mcif_wr_req_pd.get := io.rbk2mcif_wr_req_pd.get
        io.rbk2mcif_wr_req_ready.get := u_ig.io.rbk2mcif_wr_req_ready.get
        u_ig.io.rbk2mcif_wr_req_valid.get := io.rbk2mcif_wr_req_valid.get
    }

    //pdp
    if(conf.NVDLA_PDP_ENABLE) {
        u_ig.io.reg2dp_wr_weight_pdp.get := io.reg2dp_wr_weight_pdp.get
        u_ig.io.pdp2mcif_wr_req_pd.get := io.pdp2mcif_wr_req_pd.get
        io.pdp2mcif_wr_req_ready.get := u_ig.io.pdp2mcif_wr_req_ready.get
        u_ig.io.pdp2mcif_wr_req_valid.get := io.pdp2mcif_wr_req_valid.get
    }
    io.mcif2noc_axi_aw_awvalid := u_ig.io.mcif2noc_axi_aw_awvalid
    u_ig.io.mcif2noc_axi_aw_awready := io.mcif2noc_axi_aw_awready
    io.mcif2noc_axi_aw_awid := u_ig.io.mcif2noc_axi_aw_awid
    io.mcif2noc_axi_aw_awlen := u_ig.io.mcif2noc_axi_aw_awlen
    io.mcif2noc_axi_aw_awaddr := u_ig.io.mcif2noc_axi_aw_awaddr

    io.mcif2noc_axi_w_wvalid := u_ig.io.mcif2noc_axi_w_wvalid
    u_ig.io.mcif2noc_axi_w_wready := io.mcif2noc_axi_w_wready
    io.mcif2noc_axi_w_wdata := u_ig.io.mcif2noc_axi_w_wdata
    io.mcif2noc_axi_w_wstrb := u_ig.io.mcif2noc_axi_w_wstrb
    io.mcif2noc_axi_w_wlast := u_ig.io.mcif2noc_axi_w_wlast

    cq_wr_pvld := u_ig.io.cq_wr_pvld
    u_ig.io.cq_wr_prdy := cq_wr_prdy
    cq_wr_pd := u_ig.io.cq_wr_pd
    cq_wr_thread_id := u_ig.io.cq_wr_thread_id
    u_ig.io.eg2ig_axi_len := eg2ig_axi_len
    u_ig.io.eg2ig_axi_vld := eg2ig_axi_vld


    val u_cq = Module(new NV_NVDLA_MCIF_WRITE_cq)
    u_cq.io.nvdla_core_clk := io.nvdla_core_clk
    //u_cq.io.nvdla_core_rstn := io.nvdla_core_rstn
    u_cq.io.pwrbus_ram_pd := io.pwrbus_ram_pd
    cq_wr_prdy := u_cq.io.cq_wr_prdy
    u_cq.io.cq_wr_pvld := cq_wr_pvld
    u_cq.io.cq_wr_pd := cq_wr_pd
    u_cq.io.cq_wr_thread_id := cq_wr_thread_id
    for(wdma_num <- 0 until conf.WDMA_MAX_NUM) {
        cq_rd_pd(wdma_num) := u_cq.io.cq_rd_pd(wdma_num)
        cq_rd_pvld(wdma_num) := u_cq.io.cq_rd_pvld(wdma_num)
        cq_rd_prdy(wdma_num) := u_cq.io.cq_rd_prdy(wdma_num)
    }


    val u_eg = Module(new NV_NVDLA_MCIF_WRITE_eg)
    u_eg.io.nvdla_core_clk  := io.nvdla_core_clk
    //u_eg.io.nvdla_core_rstn := io.nvdla_core_rstn
    eg2ig_axi_len :=   u_eg.io.eg2ig_axi_len
    eg2ig_axi_vld :=   u_eg.io.eg2ig_axi_vld

    for(wdma_num <- 0 until conf.WDMA_MAX_NUM) {
        u_eg.io.cq_rd_pd(wdma_num) := cq_rd_pd(wdma_num)
        u_eg.io.cq_rd_pvld(wdma_num)  := cq_rd_pvld(wdma_num)
        cq_rd_prdy(wdma_num) := u_eg.io.cq_rd_prdy(wdma_num)
    }

    io.mcif2sdp_wr_rsp_complete := u_eg.io.mcif2sdp_wr_rsp_complete

    if(conf.NVDLA_BDMA_ENABLE) {
        io.mcif2bdma_wr_rsp_complete.get := u_eg.io.mcif2bdma_wr_rsp_complete.get
    }

    if(conf.NVDLA_CDP_ENABLE) {
        io.mcif2cdp_wr_rsp_complete.get := u_eg.io.mcif2cdp_wr_rsp_complete.get
    }

    if(conf.NVDLA_PDP_ENABLE) {
        io.mcif2pdp_wr_rsp_complete.get := u_eg.io.mcif2pdp_wr_rsp_complete.get
    }

    if(conf.NVDLA_RUBIK_ENABLE) {
        io.mcif2rbk_wr_rsp_complete.get := u_eg.io.mcif2rbk_wr_rsp_complete.get
    }
    u_eg.io.noc2mcif_axi_b_bvalid := io.noc2mcif_axi_b_bvalid
    io.noc2mcif_axi_b_bready := u_eg.io.noc2mcif_axi_b_bready
    u_eg.io.noc2mcif_axi_b_bid    := io.noc2mcif_axi_b_bid
}

object NV_NVDLA_MCIF_writeDriver extends App {
    implicit val conf: nvdlaConfig = new nvdlaConfig
    chisel3.Driver.execute(args, () => new NV_NVDLA_MCIF_write())
}