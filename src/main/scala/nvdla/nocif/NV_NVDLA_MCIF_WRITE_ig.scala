package nvdla

import chisel3._
import chisel3.experimental._
import chisel3.util._

//Implementation overview of ping-pong register file.

class NV_NVDLA_MCIF_WRITE_ig (implicit conf: nvdlaConfig) extends Module {
    val io = IO(new Bundle {
        //general clock
        val nvdla_core_clk = Input(Clock())      

        val pwrbus_ram_pd = Input(UInt(32.W))
        val reg2dp_wr_os_cnt = Input(UInt(8.W))
        val eg2ig_axi_len = Input(UInt(2.W))
        val eg2ig_axi_vld = Input(Bool())

        //cq_wr
        val cq_wr_pvld = Output(Bool())
        val cq_wr_prdy = Input(Bool())
        val cq_wr_thread_id = Output(UInt(3.W))
        val cq_wr_pd = Output(UInt(3.W))

        //mcif2noc
        val mcif2noc_axi_aw_awvalid = Output(Bool())
        val mcif2noc_axi_aw_awready = Input(Bool())
        val mcif2noc_axi_aw_awid = Output(UInt(8.W))
        val mcif2noc_axi_aw_awlen = Output(UInt(4.W))
        val mcif2noc_axi_aw_awaddr = Output(UInt(conf.NVDLA_MEM_ADDRESS_WIDTH.W))
        
        val mcif2noc_axi_w_wvalid = Output(Bool())
        val mcif2noc_axi_w_wready = Input(Bool())
        val mcif2noc_axi_w_wdata = Output(UInt(conf.NVDLA_PRIMARY_MEMIF_WIDTH.W))
        val mcif2noc_axi_w_wstrb = Output(UInt(conf.NVDLA_PRIMARY_MEMIF_STRB.W))
        val mcif2noc_axi_w_wlast = Output(Bool())

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

        //bdma
        val bdma2mcif_wr_req_pd          = if(conf.NVDLA_BDMA_ENABLE) Some(Input(UInt(conf.NVDLA_DMA_WR_REQ.W))) else None
        val bdma2mcif_wr_req_valid       = if(conf.NVDLA_BDMA_ENABLE) Some(Input(Bool())) else None
        val bdma2mcif_wr_req_ready       = if(conf.NVDLA_BDMA_ENABLE) Some(Output(Bool())) else None

        //rbk
        val rbk2mcif_wr_req_pd          = if(conf.NVDLA_RUBIK_ENABLE)  Some(Input(UInt(conf.NVDLA_DMA_WR_REQ.W))) else None
        val rbk2mcif_wr_req_valid       = if(conf.NVDLA_RUBIK_ENABLE) Some(Input(Bool())) else None
        val rbk2mcif_wr_req_ready       = if(conf.NVDLA_RUBIK_ENABLE) Some(Output(Bool())) else None

        //pdp
        val pdp2mcif_wr_req_pd          = if(conf.NVDLA_PDP_ENABLE)  Some(Input(UInt(conf.NVDLA_DMA_WR_REQ.W))) else None
        val pdp2mcif_wr_req_valid       = if(conf.NVDLA_PDP_ENABLE) Some(Input(Bool())) else None
        val pdp2mcif_wr_req_ready       = if(conf.NVDLA_PDP_ENABLE) Some(Output(Bool())) else None


        //cdp
        val cdp2mcif_wr_req_pd          = if(conf.NVDLA_CDP_ENABLE)  Some(Input(UInt(conf.NVDLA_DMA_WR_REQ.W))) else None
        val cdp2mcif_wr_req_valid       = if(conf.NVDLA_CDP_ENABLE) Some(Input(Bool())) else None
        val cdp2mcif_wr_req_ready       = if(conf.NVDLA_CDP_ENABLE) Some(Output(Bool())) else None
        })

    val wdma_clint_wr_req_valid = Wire(Vec(conf.WDMA_NUM,Bool()))
    val wdma_clint_wr_req_ready = Wire(Vec(conf.WDMA_NUM,Bool()))
    val wdma_clint_wr_req_pd    = Wire(Vec(conf.WDMA_NUM,UInt(conf.NVDLA_DMA_WR_REQ.W)))
    val wdma_cline_wr_weight    = Wire(Vec(conf.WDMA_NUM,UInt(8.W)))
    var wdma_index = 0

    if(conf.NVDLA_BDMA_ENABLE) {
        wdma_clint_wr_req_valid(wdma_index) := io.bdma2mcif_wr_req_valid.get
        io.bdma2mcif_wr_req_ready.get :=  wdma_clint_wr_req_ready(wdma_index)
        wdma_clint_wr_req_pd(wdma_index)    := io.bdma2mcif_wr_req_pd.get
        wdma_cline_wr_weight(wdma_index)    := io.reg2dp_wr_weight_bdma.get
        wdma_index +=1
    }

    {
        wdma_clint_wr_req_valid(wdma_index) := io.sdp2mcif_wr_req_valid
        io.sdp2mcif_wr_req_ready := wdma_clint_wr_req_ready(wdma_index)
        wdma_clint_wr_req_pd(wdma_index)    := io.sdp2mcif_wr_req_pd
        wdma_cline_wr_weight(wdma_index)    := io.reg2dp_wr_weight_sdp
        wdma_index +=1
    }

    if(conf.NVDLA_PDP_ENABLE) {
        wdma_clint_wr_req_valid(wdma_index) := io.pdp2mcif_wr_req_valid.get
        io.pdp2mcif_wr_req_ready.get := wdma_clint_wr_req_ready(wdma_index)
        wdma_clint_wr_req_pd(wdma_index)    := io.pdp2mcif_wr_req_pd.get
        wdma_cline_wr_weight(wdma_index)    := io.reg2dp_wr_weight_pdp.get
        wdma_index +=1
    }

    if(conf.NVDLA_CDP_ENABLE) {
        wdma_clint_wr_req_valid(wdma_index) := io.cdp2mcif_wr_req_valid.get
        io.cdp2mcif_wr_req_ready.get := wdma_clint_wr_req_ready(wdma_index)
        wdma_clint_wr_req_pd(wdma_index)    := io.cdp2mcif_wr_req_pd.get
        wdma_cline_wr_weight(wdma_index)    := io.reg2dp_wr_weight_cdp.get
        wdma_index +=1
    }
    if(conf.NVDLA_RUBIK_ENABLE) {
        wdma_clint_wr_req_valid(wdma_index) := io.rbk2mcif_wr_req_valid.get
        io.rbk2mcif_wr_req_ready.get := wdma_clint_wr_req_ready(wdma_index)
        wdma_clint_wr_req_pd(wdma_index)    := io.rbk2mcif_wr_req_pd.get
        wdma_cline_wr_weight(wdma_index)    := io.reg2dp_wr_weight_rbk.get
        wdma_index +=1
    }



    val arb2spt_cmd_ready = Wire(Bool())
    val arb2spt_cmd_valid = Wire(Bool())
    val arb2spt_cmd_pd = Wire(UInt((conf.NVDLA_DMA_WR_IG_PW).W))
    val arb2spt_dat_ready = Wire(Bool())
    val arb2spt_dat_valid = Wire(Bool())
    val arb2spt_dat_pd = Wire(UInt((conf.NVDLA_DMA_WR_REQ-1).W))

    val bpt2arb_cmd_valid = Wire(Vec(conf.WDMA_NUM,Bool()))
    val bpt2arb_cmd_ready = Wire(Vec(conf.WDMA_NUM,Bool()))
    val bpt2arb_cmd_pd = Wire(Vec(conf.WDMA_NUM,UInt((conf.NVDLA_DMA_WR_IG_PW).W)))
    val bpt2arb_dat_valid = Wire(Vec(conf.WDMA_NUM,Bool()))
    val bpt2arb_dat_ready = Wire(Vec(conf.WDMA_NUM,Bool()))
    val bpt2arb_dat_pd = Wire(Vec(conf.WDMA_NUM,UInt((conf.NVDLA_DMA_WR_REQ-1).W)))

    val u_bpt = Array.fill(conf.WDMA_NUM){ Module(new NV_NVDLA_MCIF_WRITE_IG_bpt)}
    for(i <- 0 until conf.WDMA_NUM){
        u_bpt(i).io.nvdla_core_clk := io.nvdla_core_clk
        u_bpt(i).io.pwrbus_ram_pd := io.pwrbus_ram_pd

        u_bpt(i).io.dma2bpt_req_valid := wdma_clint_wr_req_valid(i)
        wdma_clint_wr_req_ready(i) := u_bpt(i).io.dma2bpt_req_ready
        u_bpt(i).io.dma2bpt_req_pd := wdma_clint_wr_req_pd(i)

        bpt2arb_cmd_valid(i) := u_bpt(i).io.bpt2arb_cmd_valid
        u_bpt(i).io.bpt2arb_cmd_ready := bpt2arb_cmd_ready(i)
        bpt2arb_cmd_pd(i) := u_bpt(i).io.bpt2arb_cmd_pd

        bpt2arb_dat_valid(i) := u_bpt(i).io.bpt2arb_dat_valid
        u_bpt(i).io.bpt2arb_dat_ready := bpt2arb_dat_ready(i)
        bpt2arb_dat_pd(i) := u_bpt(i).io.bpt2arb_dat_pd
        u_bpt(i).io.axid := i.U(4.W)
    }

    val u_arb = Module(new NV_NVDLA_MCIF_WRITE_IG_arb)
    u_arb.io.nvdla_core_clk := io.nvdla_core_clk
    u_arb.io.pwrbus_ram_pd  := io.pwrbus_ram_pd
    for(i <- 0 until conf.WDMA_NUM){
        u_arb.io.bpt2arb_cmd_valid(i)  := bpt2arb_cmd_valid(i)
        bpt2arb_cmd_ready(i)        := u_arb.io.bpt2arb_cmd_ready(i)
        u_arb.io.bpt2arb_cmd_pd(i)     := bpt2arb_cmd_pd(i)

        u_arb.io.bpt2arb_dat_valid(i)  := bpt2arb_dat_valid(i)
        u_arb.io.bpt2arb_dat_pd(i)     := bpt2arb_dat_pd(i)
        bpt2arb_dat_ready(i)        := u_arb.io.bpt2arb_dat_ready(i)

        u_arb.io.reg2dp_wr_weight(i):= wdma_cline_wr_weight(i)
    }

    arb2spt_cmd_valid := u_arb.io.arb2spt_cmd_valid
    u_arb.io.arb2spt_cmd_ready := arb2spt_cmd_ready
    arb2spt_cmd_pd             := u_arb.io.arb2spt_cmd_pd
    arb2spt_dat_valid          := u_arb.io.arb2spt_dat_valid
    u_arb.io.arb2spt_dat_ready := arb2spt_dat_ready

    arb2spt_dat_pd             := u_arb.io.arb2spt_dat_pd

    val u_cvt = Module(new NV_NVDLA_MCIF_WRITE_IG_cvt)
    u_cvt.io.nvdla_core_clk     := io.nvdla_core_clk

    u_cvt.io.reg2dp_wr_os_cnt   := io.reg2dp_wr_os_cnt
    io.cq_wr_pvld               := u_cvt.io.cq_wr_pvld
    u_cvt.io.cq_wr_prdy         := io.cq_wr_prdy
    io.cq_wr_pd                 := u_cvt.io.cq_wr_pd
    io.cq_wr_thread_id          := u_cvt.io.cq_wr_thread_id

    u_cvt.io.eg2ig_axi_len      := io.eg2ig_axi_len
    u_cvt.io.eg2ig_axi_vld      := io.eg2ig_axi_vld

    u_cvt.io.spt2cvt_cmd_valid  := arb2spt_cmd_valid
    arb2spt_cmd_ready           := u_cvt.io.spt2cvt_cmd_ready
    u_cvt.io.spt2cvt_cmd_pd     := arb2spt_cmd_pd

    u_cvt.io.spt2cvt_dat_valid  := arb2spt_dat_valid
    arb2spt_dat_ready           := u_cvt.io.spt2cvt_dat_ready
    u_cvt.io.spt2cvt_dat_pd     := arb2spt_dat_pd

    io.mcif2noc_axi_aw_awvalid  := u_cvt.io.mcif2noc_axi_aw_awvalid
    u_cvt.io.mcif2noc_axi_aw_awready  := io.mcif2noc_axi_aw_awready
    io.mcif2noc_axi_aw_awid     := u_cvt.io.mcif2noc_axi_aw_awid

    io.mcif2noc_axi_aw_awlen    := u_cvt.io.mcif2noc_axi_aw_awlen
    io.mcif2noc_axi_aw_awaddr := u_cvt.io.mcif2noc_axi_aw_awaddr

    io.mcif2noc_axi_w_wvalid    := u_cvt.io.mcif2noc_axi_w_wvalid
    u_cvt.io.mcif2noc_axi_w_wready  := io.mcif2noc_axi_w_wready

    io.mcif2noc_axi_w_wdata   := u_cvt.io.mcif2noc_axi_w_wdata
    io.mcif2noc_axi_w_wstrb   := u_cvt.io.mcif2noc_axi_w_wstrb
    io.mcif2noc_axi_w_wlast   := u_cvt.io.mcif2noc_axi_w_wlast

}
object NV_NVDLA_MCIF_WRITE_igDriver extends App {
    implicit val conf: nvdlaConfig = new nvdlaConfig
    chisel3.Driver.execute(args, () => new NV_NVDLA_MCIF_WRITE_ig())
}