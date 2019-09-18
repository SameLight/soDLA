package nvdla

import chisel3._
import chisel3.util._
import chisel3.experimental._

// this is a two clock read, synchronous-write memory, with bypass

class nv_ram_rwst(dep: Int, wid: Int) extends Module{

    val io = IO(new Bundle {
        //clock
        val clk = Input(Clock())

        //control signal
        val ra = Input(UInt(wid.W))
        val re = Input(Bool())
        val dout = Output(UInt(wid.W))
        val wa = Input(UInt(wid.W))
        val we = Input(Bool())
        val di = Input(UInt(wid.W))
        val pwrbus_ram_pd = Input(UInt(32.W))
    })
withClock(io.clk){
    // assign data...
    // Create a synchronous-read, synchronous-write memory (like in FPGAs).
    val mem = SyncReadMem(dep, UInt(wid.W))
    // Create one write port and one read port.
    when (io.we) { 
        mem.write(io.wa, io.di) 
        io.dout := DontCare
    }.otherwise{ 
        val dout_ram = mem.read(io.ra, io.re)
        io.dout := RegNext(dout_ram)
    }
  }
}
