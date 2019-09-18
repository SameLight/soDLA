package nvdla

import chisel3._
import chisel3.experimental._
import chisel3.util._


class read_ig_arb extends Module {
    val io = IO(new Bundle() {
        val clk = Input(Clock())
        val req = Input(Vec(10, Bool()))
        val wt = Input(Vec(10, UInt(8.W)))
        val gnt_busy = Input(Bool())
        val gnt = Output(UInt(10.W))
    })

    val wrr_gnt_arr = List( "b0000000000",
        "b0000000001", "b0000000010", "b0000000100", "b0000001000", "b0000010000",
        "b0000100000", "b0001000000", "b0010000000", "b0100000000", "b1000000000",
    )

    val req = VecInit((0 to 9)  map { i => io.req(i) & io.wt(i).orR })

    withClock(io.clk) {
        val gnt    = RegInit(UInt(10.W), 0.U)
        val gnt_pre = RegInit(UInt(10.W), 0.U)
        val wrr_gnt = RegInit(UInt(10.W), 0.U)  // last gnt

         gnt := Fill(10, !io.gnt_busy) & gnt_pre
         io.gnt := gnt

         when(!((req.asUInt() & wrr_gnt).orR)) {
             gnt_pre := MuxLookup(wrr_gnt, 0.U(10.W),
                 (0 to 10) map { i=> wrr_gnt_arr(i).asUInt(10.W) -> MuxCase(0.U(10.W), (i until i+10) map{ j=> req(j % 10) -> (1.U << (j % 10))} ) }
             )
        } .otherwise {
            gnt_pre := wrr_gnt
        }

        when((!io.gnt_busy) & (req.asUInt() =/= 0.U(10.W))) {
            wrr_gnt := gnt
        }
    }
}
class write_ig_arb(depth: Int, width: Int) extends Module {
    val io = IO(new Bundle() {
        val clk = Input(Clock())
        val req = Input(Vec(depth, Bool()))
        val wt = Input(Vec(depth, UInt(width.W)))
        val gnt_busy = Input(Bool())
        val gnt = Output(UInt(depth.W))
    })

    val req = VecInit((0 until depth)
            map { i => io.req(i) & io.wt(i).orR })

    val new_wt_left = VecInit((0 until depth)
            map { i => io.wt(i) - 1.U })

    withClock(io.clk) {
        val gnt = Reg(UInt(depth.W))
        val gnt_pre = RegInit(UInt(depth.W), 0.U)
        val wrr_gnt = RegInit(UInt(depth.W), 0.U)
        val wt_left = RegInit(UInt(width.W), 0.U)
        val wt_left_nxt = Reg(UInt(width.W))

        gnt := Fill(depth, !io.gnt_busy) & gnt_pre
        io.gnt := gnt

        wt_left_nxt := wt_left
        when(wt_left === 0.U  | !((req.asUInt() & wrr_gnt).orR)) {
            for(i<-0 until depth) {
                when((1.U<< i) >>1=== wrr_gnt){
                        var index_cnt = MuxCase(0.U,(i to i+depth) map{index => (req(index.U % depth.U) === true.B) -> index.U % depth.U} )
                        gnt_pre := (1.U << index_cnt)
                        wt_left_nxt := new_wt_left(index_cnt)
                }
            }
        } .otherwise {
            gnt_pre := wrr_gnt
            wt_left_nxt := wt_left - 1.U
        }

        when((!io.gnt_busy) & (req.asUInt() =/= 0.U(depth.W))) {
            wrr_gnt := gnt
            wt_left := wt_left_nxt
        }
    }
}

class read_eg_arb extends Module {
    val io = IO(new Bundle() {
        val clk = Input(Clock())
        val req = Input(Vec(10, Bool()))
        val wt = Input(Vec(10, UInt(8.W)))
        val gnt = Output(UInt(10.W))
    })

    val req = VecInit((0 to 9)
      map { i =>  io.req(i) & io.wt(i).orR})


    withClock(io.clk) {
        val gnt = Reg(UInt(10.W))
        val gnt_pre = RegInit(UInt(10.W), 0.U)
        val wrr_gnt = RegInit(UInt(10.W), 0.U)

        gnt := gnt_pre
        io.gnt := gnt

        val wrr_gnt_arr = List( "b0000000000", 
                                "b0000000001", "b0000000010", "b0000000100", "b0000001000", "b0000010000", 
                                "b0000100000", "b0001000000", "b0010000000", "b0100000000", "b1000000000",
                                )

        when(!((req.asUInt() & wrr_gnt).orR)) {
            gnt_pre := MuxLookup(wrr_gnt, 0.U(10.W),
                (0 to 10) map { i=> wrr_gnt_arr(i).asUInt(10.W) -> MuxCase(0.U(10.W), (i until i+10) map{ j=> req(j % 10) -> (1.U << (j % 10))} ) }
            )
        } .otherwise {
            gnt_pre := wrr_gnt
        }

        when((req.asUInt() =/= 0.U(10.W))) {
            wrr_gnt := gnt
        }
    }
}

object read_ig_arb_Driver extends App {
    chisel3.Driver.execute(args, () => new read_ig_arb())
}


object read_eg_arb_Driver extends App {
    chisel3.Driver.execute(args, () => new read_eg_arb())
}