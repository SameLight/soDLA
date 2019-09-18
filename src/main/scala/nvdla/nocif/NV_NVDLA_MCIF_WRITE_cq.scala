package nvdla

import chisel3._
import chisel3.experimental._
import chisel3.util._

//Implementation overview of ping-pong register file.

class NV_NVDLA_MCIF_WRITE_cq(implicit conf:nvdlaConfig) extends Module {
    val io = IO(new Bundle {
        //general clock
        val nvdla_core_clk = Input(Clock())      

        //cq_wr
        val cq_wr_prdy = Output(Bool())
        val cq_wr_pvld = Input(Bool())
        val cq_wr_thread_id = Input(UInt(3.W))
        val cq_wr_pause = if(conf.FV_RAND_WR_PAUSE) Some(Input(Bool())) else None
        val cq_wr_pd = Input(UInt(3.W))
        
        val cq_rd_pd    = Output(Vec(conf.WDMA_MAX_NUM, UInt(3.W)))
        val cq_rd_pvld  = Output(Vec(conf.WDMA_MAX_NUM, Bool()))
        val cq_rd_prdy  = Output(Vec(conf.WDMA_MAX_NUM, Bool()))

        val pwrbus_ram_pd = Input(UInt(32.W))
    })

    val cq_rd_take = Wire(Bool())
    val cq_rd_pd_p = Wire(UInt(3.W))
    val cq_rd_take_thread_id = Wire(Vec(3, Bool()))

    // We also declare some per-thread flags that indicate whether to have the write bypass the internal fifo.
    // These per-class wr_bypassing* flags are set by the take-side logic. We basically pretend that we never pushed the fifo,
    // but make sure we return a credit to the sender.
    //

    val wr_bypassing = Wire(Bool())

    // Master Clock Gating (SLCG)
    //
    // We gate the clock(s) when idle or stalled.
    // This allows us to turn off numerous miscellaneous flops
    // that don't get gated during synthesis for one reason or another.
    //
    // We gate write side and read side separately.
    // If the fifo is synchronous, we also gate the ram separately, but if
    // -master_clk_gated_unified or -status_reg/-status_logic_reg is specified,
    // then we use one clk gate for write, ram, and read.
    //
    val nvdla_core_clk_mgated_skid = Wire(Clock())
    val nvdla_core_clk_mgated_skid_enable = Wire(Bool())

    val nvdla_core_clk_rd_mgate_skid = Module(new NV_CLK_gate_power)

    nvdla_core_clk_rd_mgate_skid.io.clk := io.nvdla_core_clk
    nvdla_core_clk_rd_mgate_skid.io.clk_en := nvdla_core_clk_mgated_skid_enable
    nvdla_core_clk_mgated_skid := nvdla_core_clk_rd_mgate_skid.io.clk_gated

    val nvdla_core_clk_mgated_enable = Wire(Bool()) // assigned by code at end of this module
    val nvdla_core_clk_mgated   = Wire(Clock()) // used only in synchronous fifos

    val nvdla_core_clk_mgate = Module(new NV_CLK_gate_power)
    nvdla_core_clk_mgate.io.clk := io.nvdla_core_clk
    nvdla_core_clk_mgate.io.clk_en := nvdla_core_clk_mgated_enable
    nvdla_core_clk_mgated := nvdla_core_clk_mgate.io.clk_gated

    //
    // WRITE SIDE
    //
    val wr_reserving = Wire(Bool())
    val cq_wr_busy_int = RegInit(false.B)
    io.cq_wr_prdy := !cq_wr_busy_int
    wr_reserving := io.cq_wr_pvld && !cq_wr_busy_int // reserving write space?
    val wr_popping = Wire(Bool())
    val cq_wr_count = RegInit("b0".asUInt(9.W))

    val wr_reserving_and_not_bypassing = wr_reserving && !wr_bypassing
    val wr_count_next_wr_popping = Mux(wr_reserving_and_not_bypassing, cq_wr_count, cq_wr_count-1.U)
    val wr_count_next_no_wr_popping = Mux(wr_reserving_and_not_bypassing, cq_wr_count+1.U, cq_wr_count)
    val wr_count_next = Mux(wr_popping, wr_count_next_wr_popping, wr_count_next_no_wr_popping)
    val wr_count_next_no_wr_popping_is_256 = if(wr_count_next_no_wr_popping == 256.U)  true.B else false.B
    val wr_count_next_is_256 = Mux(wr_popping,  true.B , wr_count_next_no_wr_popping_is_256)

    val wr_limit_override_value = RegInit("b0".asUInt(9.W))
    val wr_limit_muxed = wr_limit_override_value
    val wr_limit_reg = wr_limit_muxed
    val cq_wr_busy_next = Wire(Bool())

    if(conf.FV_RAND_WR_PAUSE) {
       cq_wr_busy_next := wr_count_next_is_256 || ((wr_limit_reg =/= 0.U(9.W)) && (wr_count_next >= wr_limit_reg)) || io.cq_wr_pause.get
    } else {
       cq_wr_busy_next := wr_count_next_is_256 || ((wr_limit_reg =/= 0.U(9.W)) && (wr_count_next >= wr_limit_reg))
    }

    withClock(nvdla_core_clk_mgated) {
        cq_wr_busy_int :=  cq_wr_busy_next
        when(wr_reserving_and_not_bypassing^wr_popping) {
            cq_wr_count := wr_count_next
        }
    }

     val wr_pushing = wr_reserving && !wr_bypassing // data pushed same cycle as cq_wr_pvld
     val wr_pushing_thread_id = io.cq_wr_thread_id // thread being written

    //
    // RAM
    //
    val wr_adr_popping = wr_pushing
    val cq_wr_adr   = Wire(UInt(8.W))
    val cq_rd_adr = RegInit("b0".asUInt(8.W))
    val cq_rd_adr_p = cq_rd_adr
    val rd_enable   = Wire(Bool())
    //val pwrbus_ram_pd = Wire(UInt(32.W))

    // Adding parameter for fifogen to disable wr/rd contention assertion in ramgen.
    // Fifogen handles this by ignoring the data on the ram data out for that cycle.

    val ram = Module(new nv_ram_rws(256, 3))
    ram.io.clk := io.nvdla_core_clk

    ram.io.wa  := cq_wr_adr
    ram.io.we  := wr_pushing
    ram.io.di  := io.cq_wr_pd
    ram.io.ra  := cq_rd_adr_p
    ram.io.re  := rd_enable
    cq_rd_pd_p  := ram.io.dout

    //
    // SYNCHRONOUS BOUNDARY
    //
    val rd_popping = Wire(Bool())
    val rd_pushing = wr_pushing
    val rd_pushing_thread_id = wr_pushing_thread_id
    val rd_pushing_adr = cq_wr_adr
    //
    // MULTITHREADED FREE LIST FIFO
    //
    // free list of cq_wr_adr's from read side to write side
    // these are passed in a ff fifo when the fifo is popped
    //
    // there's an extra mux of the internal flops that is
    // used to determine which address to use when
    // rd_pushing is 1 if the fifo is async.
    //

    val rd_popping_adr = Wire(UInt(8.W))
    val free_adr_index = Wire(UInt(8.W))
    val free_adr_mask_next = RegInit(VecInit(Seq.fill(256)(false.B)))
    val free_adr_mask = RegInit(VecInit(Seq.fill(256)(false.B)))

    when(rd_popping) {
        free_adr_mask_next(rd_popping_adr) := Mux(rd_popping_adr === free_adr_index,  true.B ,false.B)
    }

    withClock(nvdla_core_clk_mgated){
        when (rd_popping || wr_adr_popping) {
            free_adr_mask := free_adr_mask_next
        }
    }

    val flag_l0 = Wire(Vec(127,Bool()))
    val flag_l1 = Wire(Vec(63,Bool()))
    val flag_l2 = Wire(Vec(31,Bool()))
    val flag_l3 = Wire(Vec(15,Bool()))
    val flag_l4 = Wire(Vec(7,Bool()))
    val flag_l5 = Wire(Vec(3,Bool()))
    val flag_l6 = Wire(Bool())

    val index_l0 = Wire(Vec(128,Bool()))
    val index_l1 = Wire(Vec(64,UInt(2.W)))
    val index_l2 = Wire(Vec(32,UInt(3.W)))
    val index_l3 = Wire(Vec(16,UInt(4.W)))
    val index_l4 = Wire(Vec(8,UInt(5.W)))
    val index_l5 = Wire(Vec(4,UInt(6.W)))
    val index_l6 = Wire(Vec(2,UInt(7.W)))
    val index_l7 = Wire(UInt(8.W))
    cq_wr_adr := free_adr_index
    for( index <- 0 until 127){
        flag_l0(index) := free_adr_mask(index*2+1) | free_adr_mask(index*2)
    }

    for( index <- 0 until 63){
        flag_l1(index) := flag_l0(index*2+1) | flag_l0(index*2)
    }

    for( index <- 0 until 31){
        flag_l2(index) := flag_l1(index*2+1) | flag_l1(index*2)
    }

    for( index <- 0 until 15){
        flag_l3(index) := flag_l2(index*2+1) | flag_l2(index*2)
    }
    for( index <- 0 until 7){
        flag_l4(index) := flag_l3(index*2+1) | flag_l3(index*2)
    }
    for( index <- 0 until 3){
        flag_l5(index) := flag_l4(index*2+1) | flag_l4(index*2)
    }
    flag_l6:= flag_l5(0)|flag_l5(1)

    for( index <- 0 to 127){
        index_l0(index) := ! free_adr_mask(2*index)
    }

    for( index <- 0 to 63){
        index_l1(index) := Cat(!flag_l0(2*index),Mux(flag_l0(2*index),index_l0(2*index),index_l0(2*index+1)))
    }

    for( index <- 0 to 31){
        index_l2(index) := Cat(!flag_l1(2*index),Mux(flag_l1(2*index),index_l1(2*index),index_l1(2*index+1)))
    }

    for( index <- 0 to 15){
        index_l3(index) := Cat(!flag_l2(2*index),Mux(flag_l2(2*index),index_l2(2*index),index_l2(2*index+1)))
    }

    for( index <- 0 to 7){
        index_l4(index) := Cat(!flag_l3(2*index),Mux(flag_l3(2*index),index_l3(2*index),index_l3(2*index+1)))
    }

    for( index <- 0 to 3){
        index_l5(index) := Cat(!flag_l4(2*index),Mux(flag_l4(2*index),index_l4(2*index),index_l4(2*index+1)))
    }

    for( index <- 0 to 1){
        index_l6(index) := Cat(!flag_l5(2*index),Mux(flag_l5(2*index),index_l5(2*index),index_l5(2*index+1)))
    }

    index_l7 := Cat(!flag_l6(0),Mux(flag_l6(0),index_l6(0),index_l6(0)))

    free_adr_index := index_l7

    wr_popping := rd_popping

    //
    // READ SIDE
    //

    //
    // credits for taker are simply rd_pushing*
    //
    val cq_rd_credit = RegInit(VecInit(Seq.fill(conf.WDMA_MAX_NUM)(false.B)))
    val rd_pushing_q = RegInit(false.B)
    withClock(nvdla_core_clk_mgated) {
        when(rd_pushing || rd_pushing_q) {
            for(index <-0 to 4) {
                cq_rd_credit(index) := rd_pushing =/= 0.U //&& (rd_pushing_thread_id === index.U(3.W))
            }
        }
    }
    val rd_pushing_vec = Wire(Vec(5,Bool()))
    val rd_take = Wire(Vec(5,Bool()))
    for(index <-0 to 4) {
        rd_pushing_vec(index) := rd_pushing && (rd_pushing_thread_id === index.U(3.W))
        rd_take(index)    := cq_rd_take && (cq_rd_take_thread_id.asUInt === index.U(3.W))
    }

    val head = RegInit(VecInit(Seq.fill(5)(false.B)))
    val tail = RegInit(VecInit(Seq.fill(5)(false.B)))
    val rd_take_n_dly = RegInit("b0".asUInt(5.W))
    val rd_take_dly_cg = RegInit(false.B)
    val update_rd_take_n_dly = cq_rd_take || rd_take_dly_cg

    withClock(nvdla_core_clk_mgated) {
        rd_take_dly_cg := cq_rd_take
        when(update_rd_take_n_dly) {
            rd_take_n_dly := Cat(rd_take(4),rd_take(3),rd_take(2),rd_take(1),rd_take(0))
        }
    }

    val adr_ram_wr_adr = Wire(UInt(8.W))
    val adr_ram_wr_data = Wire(UInt(8.W))
    val adr_ram_wr_enable = Wire(Bool())

    val adr_ram_rd_adr = RegInit("b0".asUInt(8.W))
    val adr_ram_rd_data = Wire(UInt(8.W))
    val adr_ram_rd_enable = RegInit(false.B)

    val cq_rd_count = RegInit(VecInit(Seq.fill(5)(false.B)))
    val rd_count_next = Wire(Vec(5,UInt(9.W)))
    for(index <- 0 to 4) {
        rd_count_next(index) := Mux(rd_pushing_vec(index),
                                    Mux(rd_take(index),cq_rd_count(index),cq_rd_count(index)+1.U),
                                    Mux(rd_take(index),cq_rd_count(index)-1.U,cq_rd_count(index)))
    }

    withClock(nvdla_core_clk_mgated) {
        for(index <- 0 to 4) {
            when(rd_pushing_vec(index)^rd_take(index)) {
                cq_rd_count(index) := rd_count_next(index)
            }
        }
    }

    val update_head = RegInit("b0".asUInt(5.W))
    val update_head_next =  Wire(Vec(conf.WDMA_MAX_NUM,Bool()))
    for(index <- 0 to 4) {
        update_head_next(index) := Mux(rd_take(index)&&cq_rd_count(index)> 1.U,
                                        true.B,false.B)
    }

    withClock(nvdla_core_clk_mgated) {
        when(rd_pushing || cq_rd_take) {
            update_head := update_head_next.asUInt
        }
        for(index <- 0 to 4) {
            when(rd_pushing) {
                tail(index) := rd_pushing_adr
            }

            when((rd_pushing && cq_rd_count(index) === 0.U) ||
                (rd_pushing &&rd_take(index) && cq_rd_count(index) === 1.U)) {
                     head(index) := rd_pushing_adr
                 } .elsewhen(update_head(index)) {
                     head(index) := adr_ram_rd_data
                 }
        }
    }
    val adr_ram = Module(new nv_ram_rwst(256,8))
    adr_ram.io.clk := io.nvdla_core_clk
    adr_ram.io.wa  := adr_ram_wr_adr
    adr_ram.io.we  := adr_ram_wr_enable
    adr_ram.io.di  := adr_ram_wr_data
    adr_ram.io.ra  := adr_ram_rd_adr
    adr_ram.io.re  := adr_ram_rd_enable
    adr_ram_rd_data:= adr_ram.io.dout
    adr_ram.io.pwrbus_ram_pd := io.pwrbus_ram_pd

    adr_ram_wr_data := rd_pushing_adr

    when(rd_pushing_thread_id >= 0.U && rd_pushing_thread_id <= 4.U) {
        adr_ram_wr_adr := tail(rd_pushing_thread_id)
        adr_ram_wr_enable := Mux(rd_pushing && cq_rd_count(rd_pushing_thread_id) =/= 0.U,true.B,false.B)
        adr_ram_rd_enable := Mux(cq_rd_take && cq_rd_count(cq_rd_take_thread_id.asUInt) =/= 0.U,true.B,false.B)
        adr_ram_rd_adr :=  Mux(rd_take_n_dly(cq_rd_take_thread_id.asUInt) && update_head(cq_rd_take_thread_id.asUInt) =/= 0.U,
                                adr_ram_rd_data,head(cq_rd_take_thread_id.asUInt))
        cq_rd_adr   := Mux(rd_take_n_dly(cq_rd_take_thread_id.asUInt) && update_head(cq_rd_take_thread_id.asUInt) =/= 0.U,
                                adr_ram_rd_data,head(cq_rd_take_thread_id.asUInt))
    } .otherwise{
        adr_ram_wr_adr := 0.U(8.W)
        adr_ram_wr_enable := false.B
        adr_ram_rd_enable := false.B
        adr_ram_rd_adr := 0.U(8.W)
        cq_rd_adr      := 0.U(8.W)
    }

    val rd_take_dly = RegInit(false.B)
    rd_popping := rd_take_dly

    val rd_adr_dly = RegInit("b0".asUInt(8.W))
    rd_popping_adr := rd_adr_dly
    rd_enable   := cq_rd_take
    withClock(nvdla_core_clk_mgated) {
        rd_take_dly := cq_rd_take
        when( cq_rd_take) {
            rd_adr_dly := cq_rd_adr
        }
    }

    //
    // -rd_take_to_rd_busy conversion (conceptually outside the fifo except for ra2 bypass)
    //
    val cq_rd_take_elig  = Wire(Vec(conf.WDMA_MAX_NUM,Bool()))  // mask of threads that can do takes this cycle
    val rd_pre_bypassing = Wire(Vec(5,Bool()))
    val rd_bypassing     = Wire(Vec(5,Bool()))
    val rd_skid         = RegInit(VecInit(Seq.fill(3*5)(false.B)))
    val rd_skid_vld     = RegInit(VecInit(Seq.fill(3*5)(false.B)))
    val cq_rd_prdy_d    = RegInit(VecInit(Seq.fill(5)(true.B)))

    val cq_rd_credits   = RegInit(VecInit(Seq.fill(5)(false.B)))
    val cq_rd_credits_ne= RegInit(VecInit(Seq.fill(5)(false.B)))
    val cq_rd_credits_w_take_next  = Wire(Vec(5,UInt(9.W)))
    val cq_rd_credits_wo_take_next = Wire(Vec(5,UInt(9.W)))
    val cq_rd_credits_next         = Wire(Vec(5,UInt(9.W)))

    for(index <- 0 to 4) {
        io.cq_rd_pvld(index) := rd_skid(index*3) || rd_pre_bypassing(index);  			// full bypass for 0-latency
        io.cq_rd_pd(index) := Mux(rd_skid_vld(index*3),rd_skid(index*3),io.cq_wr_pd)
        cq_rd_credits_w_take_next(index) := cq_rd_credits(index)+cq_rd_credit(index) - 1.U
        cq_rd_credits_wo_take_next(index) := cq_rd_credits(index)+cq_rd_credit(index)
        cq_rd_credits_next(index) := Mux(rd_take(index), cq_rd_credits_w_take_next(index),cq_rd_credits_wo_take_next(index))
        cq_rd_take_elig(index) := cq_rd_prdy_d(index) || !rd_skid_vld(index*3+0) || !rd_skid_vld(index*3+1) ||
                                (! rd_skid_vld(index*3+2) && !rd_take_n_dly(index))&&cq_rd_credit(index)||cq_rd_credits_ne(index)
        rd_pre_bypassing(index) := io.cq_wr_pvld && !cq_wr_busy_int &&(io.cq_wr_thread_id === index.U)&& (cq_rd_credits(index) === 0.U)&&cq_rd_credit(index)===0.U&&(rd_take_n_dly(index)===0.U||rd_skid_vld(index))
        rd_bypassing(index) :=  rd_pre_bypassing(index) &&(rd_skid_vld(3*index+2)===0.U||rd_skid_vld(3*index+1)===0.U||(cq_rd_prdy_d(index)===0.U&&rd_skid_vld(3*index+0)&&rd_skid_vld(3*index+1))&&(rd_take_n_dly(index)===0.U) === false.B)
    }
    wr_bypassing := rd_bypassing(0) || rd_bypassing(1) || rd_bypassing(2) || rd_bypassing(3) || rd_bypassing(4)

    withClock(io.nvdla_core_clk) {
        for(index <- 0 to 4) {
           io.cq_rd_prdy(index) :=  cq_rd_prdy_d(index)
        }
    }
    cq_rd_take :=  cq_rd_take_elig.asUInt.orR()

    val cq_rd_take_thread_id_last =  RegInit("b0".asUInt(3.W))
    val cq_rd_take_thread_id_is_1 = Cat(
        cq_rd_take_elig(1)&& cq_rd_take_thread_id_last === 4.U && !cq_rd_take_elig(0),
        cq_rd_take_elig(1)&& cq_rd_take_thread_id_last === 3.U && !cq_rd_take_elig(4) && !cq_rd_take_elig(0),
        cq_rd_take_elig(1)&& cq_rd_take_thread_id_last === 2.U && !cq_rd_take_elig(3) && !cq_rd_take_elig(4) && !cq_rd_take_elig(0),
        cq_rd_take_elig(1)&& cq_rd_take_thread_id_last === 1.U && !cq_rd_take_elig(2) && !cq_rd_take_elig(3) && !cq_rd_take_elig(4) && !cq_rd_take_elig(0),
        cq_rd_take_elig(1)&& cq_rd_take_thread_id_last === 0.U )

    val cq_rd_take_thread_id_is_2 = Cat(
        cq_rd_take_elig(2) && cq_rd_take_thread_id_last === 4.U && !cq_rd_take_elig(0) && !cq_rd_take_elig(1),
        cq_rd_take_elig(2) && cq_rd_take_thread_id_last === 3.U && !cq_rd_take_elig(4) && !cq_rd_take_elig(0) && !cq_rd_take_elig(1),
        cq_rd_take_elig(2) && cq_rd_take_thread_id_last === 2.U && !cq_rd_take_elig(3) && !cq_rd_take_elig(4) && !cq_rd_take_elig(0) && !cq_rd_take_elig(1),
        cq_rd_take_elig(2) && cq_rd_take_thread_id_last === 1.U,
        cq_rd_take_elig(2) && cq_rd_take_thread_id_last === 0.U && !cq_rd_take_elig(1))

    val cq_rd_take_thread_id_is_3 = Cat (
        cq_rd_take_elig(3) && cq_rd_take_thread_id_last === 4.U && !cq_rd_take_elig(0) && !cq_rd_take_elig(1) && !cq_rd_take_elig(2),
        cq_rd_take_elig(3) && cq_rd_take_thread_id_last === 3.U && !cq_rd_take_elig(4) && !cq_rd_take_elig(0) && !cq_rd_take_elig(1) && !cq_rd_take_elig(2),
        cq_rd_take_elig(3) && cq_rd_take_thread_id_last === 2.U,
        cq_rd_take_elig(3) && cq_rd_take_thread_id_last === 1.U && !cq_rd_take_elig(2),
        cq_rd_take_elig(3) && cq_rd_take_thread_id_last === 0.U && !cq_rd_take_elig(1) && !cq_rd_take_elig(2));

    val cq_rd_take_thread_id_is_4 = Cat (
        cq_rd_take_elig(4) && cq_rd_take_thread_id_last === 4.U && !cq_rd_take_elig(0) && !cq_rd_take_elig(1) && !cq_rd_take_elig(2) && !cq_rd_take_elig(3),
        cq_rd_take_elig(4) && cq_rd_take_thread_id_last === 3.U,
        cq_rd_take_elig(4) && cq_rd_take_thread_id_last === 2.U && !cq_rd_take_elig(3),
        cq_rd_take_elig(4) && cq_rd_take_thread_id_last === 1.U && !cq_rd_take_elig(2) && !cq_rd_take_elig(3),
        cq_rd_take_elig(4) && cq_rd_take_thread_id_last === 0.U && !cq_rd_take_elig(1) && !cq_rd_take_elig(2) && !cq_rd_take_elig(3))


    cq_rd_take_thread_id(0) := Cat(cq_rd_take_thread_id_is_1,cq_rd_take_thread_id_is_3).asUInt.orR()

    cq_rd_take_thread_id(1) := Cat(cq_rd_take_thread_id_is_2,cq_rd_take_thread_id_is_3).asUInt.orR()

    cq_rd_take_thread_id(2) := cq_rd_take_thread_id_is_4.orR()

    withClock(nvdla_core_clk_mgated_skid) {
        for (wdma_num <- 0 to 4) {
            when ( (rd_bypassing(wdma_num) || rd_take_n_dly(wdma_num)) && (!rd_skid_vld(wdma_num*3+0)
                    || (io.cq_rd_pvld(wdma_num) && io.cq_rd_prdy(wdma_num) && !rd_skid_vld(wdma_num*3+1)))) {
                rd_skid(wdma_num*3+0) := Mux(rd_take_n_dly(wdma_num),cq_rd_pd_p,io.cq_wr_pd)
            }.elsewhen( io.cq_rd_pvld(wdma_num) && io.cq_rd_prdy(wdma_num) && rd_skid_vld(wdma_num*3+1) ) {
                rd_skid(wdma_num*3+0) := rd_skid(wdma_num*3+1)
            }

            when ( (rd_bypassing(wdma_num) || rd_take_n_dly(wdma_num)) && (!rd_skid_vld(wdma_num*3+1)
                    || (io.cq_rd_pvld(wdma_num) && io.cq_rd_prdy(wdma_num) && !rd_skid_vld(wdma_num*3+2)))) {
                rd_skid(wdma_num*3+1) := Mux(rd_bypassing(wdma_num) ,io.cq_wr_pd,cq_rd_pd_p)
            }.elsewhen( io.cq_rd_pvld(wdma_num) && io.cq_rd_prdy(wdma_num) && rd_skid_vld(wdma_num*3+1) ) {
                rd_skid(wdma_num*3+1) := rd_skid(wdma_num*3+2)
            }

            when ( (rd_bypassing(wdma_num) || rd_take_n_dly(wdma_num)) && rd_skid_vld(wdma_num*3+0)&&rd_skid_vld(wdma_num*3+1)
                &&(rd_skid_vld(wdma_num*3+2) ||(io.cq_rd_pvld(wdma_num) && io.cq_rd_prdy(wdma_num))===true.B)) {
                rd_skid(wdma_num*3+2) := Mux(rd_bypassing(wdma_num) ,io.cq_wr_pd,cq_rd_pd_p)
            }

            rd_skid_vld(wdma_num*3+0) := Mux(io.cq_rd_pvld(wdma_num) && io.cq_rd_prdy(wdma_num),
                                        (rd_skid_vld(wdma_num*3+1)|| (rd_bypassing(wdma_num) &&rd_skid_vld(wdma_num*3+0))||rd_take_n_dly(wdma_num)),
                                        (rd_skid_vld(wdma_num*3+0)|| rd_bypassing(wdma_num)||rd_take_n_dly(wdma_num)))

            rd_skid_vld(wdma_num*3+1) := Mux(io.cq_rd_pvld(wdma_num) && io.cq_rd_prdy(wdma_num),
                                        (rd_skid_vld(wdma_num*3+2)|| (rd_bypassing(wdma_num) &&rd_skid_vld(wdma_num*3+1))||rd_take_n_dly(wdma_num)),
                                        (rd_skid_vld(wdma_num*3+1)|| rd_bypassing(wdma_num)||rd_take_n_dly(wdma_num)))

            when(cq_rd_credit(wdma_num) || rd_take(wdma_num)) {
                cq_rd_credits(wdma_num) := cq_rd_credits_next(wdma_num)
                cq_rd_credits_ne(wdma_num) := Mux(rd_take(wdma_num),cq_rd_credits_w_take_next(wdma_num) =/= 0.U,cq_rd_credits_wo_take_next(wdma_num) =/= 0.U)
            }
        }

    }
 nvdla_core_clk_mgated_enable := ((wr_reserving || wr_pushing || wr_popping || (io.cq_wr_pvld && !cq_wr_busy_int) ||
                                    (cq_wr_busy_int =/= cq_wr_busy_next) || rd_popping) || (rd_pushing || cq_rd_take ||
                                    cq_rd_credit.asUInt =/= 0.U || rd_take_dly))
    for( i <- 0 to 4) {
        nvdla_core_clk_mgated_skid_enable := (io.cq_rd_pvld(i)&&io.cq_rd_prdy(i)||rd_bypassing(i)).orR()
    }
    nvdla_core_clk_mgated_skid_enable := nvdla_core_clk_mgated_enable.orR()
}



object NV_NVDLA_MCIF_WRITE_cqDriver extends App {
    implicit val conf: nvdlaConfig = new nvdlaConfig
    chisel3.Driver.execute(args, () => new NV_NVDLA_MCIF_WRITE_cq())
}