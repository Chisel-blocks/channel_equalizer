// Dsp-block channel_equalizer
// Description here
// Inititally written by dsp-blocks initmodule.sh, 20190429
package channel_equalizer

import chisel3._
import chisel3.experimental._
import chisel3.util._
import dsptools.{DspTester, DspTesterOptionsManager, DspTesterOptions}
import dsptools.numbers._
import breeze.math.Complex
import scala.math._
import complex_reciprocal._
import memblock._

class channel_equalizer_io( 
        n : Int, 
        symbol_length : Int, 
        users : Int
    ) extends Bundle {
        val A = Input(DspComplex(
                SInt((n).W),
                SInt((n).W)
            ) 
        )
        val Z = Output(DspComplex(
                SInt((n).W),
                SInt((n).W)
            ) 
        )
        val  reference_in= Input(DspComplex(
                SInt(n.W),
                SInt(n.W)
            )
        )
        val  reference_out= Input(DspComplex(
                SInt(n.W),
                SInt(n.W)
            )
        )
        val  estimate_in= Input(Vec(users,DspComplex(
                FixedPoint((2*n).W,(n).BP),
                FixedPoint((2*n).W,(n).BP)
            ))
        )
        val  estimate_out= Output(Vec(users,DspComplex(
                FixedPoint((2*n).W,(n).BP),
                FixedPoint((2*n).W,(n).BP)
            )) 
        )

        val estimate_format= Input(UInt(1.W))
        
        val reference_read_addr=Input(UInt(log2Ceil(symbol_length).W))
        val reference_read_enable=Input(Bool())
        val reference_write_addr=Input(UInt(log2Ceil(symbol_length).W))
        val reference_write_en=Input(Bool())
        val reference_mode=Input(UInt(2.W)) //0 internal read, 1 write in, read out

        val estimate_read_enable=Input(Bool())
        val estimate_read_addr=Input(UInt(log2Ceil(symbol_length).W))
        val estimate_write_en=Input(Bool())
        val estimate_write_addr=Input(UInt(log2Ceil(symbol_length).W))
        val estimate_mode=Input(UInt(2.W)) //0 internal read/write, 1 write in, read out

        val equalize_sync=Input(Bool()) //Rising edge resets the bin counters
        val estimate_sync=Input(Bool()) //Rising edge resets the bin counters
        

        override def cloneType = (new channel_equalizer_io(
                n=n,
                symbol_length=symbol_length,
                users=users
            )
        ).asInstanceOf[this.type]
   }

class channel_equalizer( 
        n : Int, 
        symbol_length : Int, 
        users : Int
    ) extends Module {
        val io = IO(new channel_equalizer_io( 
            n=n,
            symbol_length=symbol_length,
            users=users
        )
    )
    val reciprocal=Module( new complex_reciprocal(
            w=n,
            b=n/2
        )
    ).io

    val reference_mem=Module( new memblock(proto=io.reference_in.cloneType, symbol_length))
    val estimate_mem=Module( new memblock(proto=io.estimate_in.cloneType, symbol_length))

    val reference_read_edge= Module(new edge_detector()).io 
    reference_read_edge.A=io.reference_read_enable 
    val reference_write_edge= Module(new edge_detector()).io 
    reference_write_edge.A=io.reference_write_enable 
    val estimate_read_edge= Module(new edge_detector()).io 
    estimate_read_edge.A=io.estimate_read_enable 
    val estimate_write_edge= Module(new edge_detector()).io 
    estimate_write_edge.A=io.estimate_write_enable 
    val equalize_sync_edge= Module(new edge_detector()).io 
    equalize_sync_edge.A=io.equalize_sync_edge
    val estimate_sync_edge= Module(new edge_detector()).io 
    estimate_sync_edge.A=io.estimate_sync_edge

    // Signal IO registers
    val r_A=RegInit(0.U.asTypeOf(io.A.cloneType))
    val r_equalized=RegInit(0.U.asTypeOf(reciprocal.Q.cloneType))
    val r_Z=RegInit(0.U.asTypeOf(io.Z))
    // Registers for reference IO
    val r_reference_in=RegInit(0.U.asTypeOf(io.reference_in.cloneType))
    val r_reference_write_addr=RegInit(0.U.asTypeOf(io.reference_write_addr.cloneType))
    val r_reference_read_addr=RegInit(0.U.asTypeOf(io.reference_read_addr.cloneType))
    val r_reference_write_en=RegInit(0.U.asTypeOf(io.reference_write_en.cloneType))
    val r_reference_read_val=RegInit(0.U.asTypeOf(io.reference_out.cloneType))

    // Registers for estimate IO
    val r_estimate_in=RegInit(0.U.asTypeOf(io.estimate_in.cloneType))
    val r_estimate_write_addr=RegInit(0.U.asTypeOf(io.estimate_write_addr.cloneType))
    val r_estimate_read_addr=RegInit(0.U.asTypeOf(io.estimate_read_addr.cloneType))
    val r_estimate_write_en=RegInit(0.U.asTypeOf(io.estimate_write_en.cloneType))
    val r_estimate_read_val=RegInit(0.U.asTypeOf(io.estimate_out.cloneType))

    val r_N=RegInit(0.U.asTypeOf(reciprocal.N.cloneType))
    val r_D=RegInit(0.U.asTypeOf(reciprocal.D.cloneType))
    val r_estimate_format=RegInit(0.U.asTypeOf(io.estimate_format))

    //Register assignments
    r_A:=io.A
    r_estimate_format:=io.estimate_format
    r_reference_in:=io.reference_in
    r_reference_write_addr:=io.reference_write_addr
    r_reference_read_addr:=io.reference_read_addr
    r_estimate_in:=io.estimate_in
    r_estimate_write_addr:=io.estimate_write_addr

    // Mem input defaults
    reference_mem.write_addr=r_reference_write_addr
    reference_mem.write_val=r_reference_in
    reference_mem.write_en=false.B
    estimate_mem.write_addr=r_estimate_write_addr
    estimate_mem.write_val=r_restimate_in
    estimate_mem.write_en=false.B

   
    // State machine for channel estimation and equalization
    val equalizing:: estimating :: updating :: reading:: Nil = Enum(4)

    val state=RegInit(equalizing)  // The default state
    
    val w_address_counter_reset=Wire(Bool())
    val address_counter=withReset(w_address_counter_reset){RegInit(0.U(log2Ceil(symbol_length).W))}
    when(reset){
        w_address_counter_reset:=true.B
    }.elsewhen( equalize_sync_edge.rising || estimate_sync_edge_rising ){
        w_address_counter_reset:=true.B
    }.otherwise{
        w_address_counter_reset:=false.B
        address_counter:=address_counter+1.U
    }

    // State machine
    when ( state === equalizing ){
        // State operation
        reference_mem.read_addr:=address_counter
        reference_mem.write_en:=false.B
        estimate_mem.read_addr:=address_counter
        estimate_mem.write_en:=false.B

        // State transition
        when( reference_write_edge.rising || estimate_write_edge.rising  ) {
            state=updating
        }.elsewhen ( reference_read_edge.rising || estimate_read_edge_rising) {
            state=reading 
        }.elsewhen ( estimate_sync_edge.rising ) {
            state=estimating
        }. otherwise {
            state=equalizing
        }
    }.elsewhen( state==updating ) {
        // State operation
        when ( r_reference_write_enable ) {
            reference_mem.write_addr:=r_reference_write_addr
            reference_mem.write_val:=r_reference_write_val
            reference_mem.write_en:=true.B 
        }
        when ( r_estimate_write_enable ) {
            estimate_mem.write_addr:=r_estimate_write_addr
            estimate_mem.write_val:=r_estimate_write_val
            estimate_mem.write_en:=true.B 
        }
        // State transition
        when (( ! r_reference_write_enable ) && (! r_estimate_write_enable )) {
            state:=equalizing
        }.otherwise{
            state:=updating
        }
    }.elsewhen( state==reading ) {
        // State operation
        when ( r_reference_read_enable ) {
            reference_mem.read_addr:=r_reference_read_addr
            r_reference_read_val:= reference_mem.read_val
        }
        when ( r_estimate_read_enable ) {
            estimate_mem.read_addr:=r_estimate_read_addr
            r_estimate_read_val:= estimate_mem.read_val
        }
        // State transition
        when (( ! r_reference_read_enable ) && (! r_estimate_read_enable )) {
            state:=equalizing
        }.otherwise{
            state:=reading
        }
    }.elsewhen( state==estimating ) {
        reference_mem.read_addr:=address_counter
        reference_mem.write_en:=false.B
        estimate_mem.write_addr:=address_counter
        estimate_mem.write_en:=true.B
        estimate_mem.write_val.real:=reciprocal.Q.asTypeOf(estimate_mem.write_val).real
        estimate_mem.write_val.imag:=reciprocal.Q.asTypeOf(estimate_mem.write_val).imag

        // State transition
        when ( address_counter.toInt < log2Ceil(symbol_length) ) {
            state:=estimating
        }.otherwise{
            state:=equalizing
        }
    }.otherwise{
        state:=equalizing
    }

    //Reciprocal connections
    reciprocal.N:=r_N
    reciprocal.D:=r_D
    when (r_estimate_format === 0.U) {
        // Reciprocal channel response.
        // Compensation required to get the reference
        // for local beamforming
        r_N:=reference_mem.read_val.asTypeOf(reciprocal.N.cloneType)
        r_D:=r_A.asTypeOf(reciprocal.D.cloneType)
    }.elsewhen(r_estimate_format === 1.U) {
        // Channel response. 
        // Attenuation relative to reference
        r_N:=r_A.conj().asTypeOf(reciprocal.N.cloneType)
        r_D.real:=reference_mem.read_val.asTypeOf(reciprocal.D.cloneType).real
        r_D.imag:=reference_mem_read_val.asTypeOf(reciprocal.D.cloneType).imag
    }.otherwise {
        // Reciprocal format.
        // Compensation required to get the reference
        // for local beamforming
        r_N:=reference_mem.read_val.asTypeOf(reciprocal.N.cloneType)
        r_D:=r_A.asTypeOf(reciprocal.D.cloneType)
    }

    
    // Equalization and rounding
    r_equalized:=reciprocal.Q*r_A.asTypeOf(reciprocal.D.cloneType)
    r_Z.real:=(r_equalized.real << n/2).round.asSInt
    r_Z.imag:=(r_equalized.imag << n/2).round.asSInt

    // Output assignments
    io.reference_out:=r_reference_read_val
    io.estimate_out:=r_estimate_read_val
  
    io.Z.real:=r_Z
    io.Z.imag:=r_Z
}

//This gives you verilog
object channel_equalizer extends App {
  // Getopts parses the "Command line arguments for you"  
  def getopts(options : Map[String,String], 
      arguments: List[String]) : (Map[String,String], List[String]) = {
      //This the help
      val usage = """
          |Usage: channel_equalizer.channel_equalizer [-<option>]
          |
          | Options
          |     n              [Int]    : Number of input bits. Default 16
          |     symbol_length  [Int]    : Channel length.       Default 64         
          |     users          [Int]    : Number of users.      Default 16
          |     h                       : This help 
        """.stripMargin
      val optsWithArg: List[String]=List(
          "-n",
          "-symbol_length",
          "-users"
      )
      //Handling of flag-like options to be defined 
      arguments match {
          case "-h" :: tail => {
              println(usage)
              val (newopts, newargs) = getopts(options, tail)
              sys.exit
              (Map("h"->"") ++ newopts, newargs)
          }
          case option :: value :: tail if optsWithArg contains option => {
             val (newopts, newargs) = getopts(
                 options++Map(option.replace("-","") -> value), tail
             )
             (newopts, newargs)
          }
          case argument :: tail => {
               val (newopts, newargs) = getopts(options,tail)
               (newopts, argument.toString +: newargs)
            }
          case Nil => (options, arguments)
      }
  }
   
  // Default options
  val defaultoptions : Map[String,String]=Map(
      "n"->"16",
      "symbol_length"->"64",
      "users"->"16"
      ) 
  // Parse the options
  val (options,arguments)= getopts(defaultoptions,args.toList)

    chisel3.Driver.execute(arguments.toArray, () => new channel_equalizer(
            n=options("n").toInt,
            symbol_length=options("symbol_length").toInt,
            users=options("users").toInt
        )
    )
}

//[TODO] Modify test to use parameters
//This is a simple unit tester for demonstration purposes
//class unit_tester(c: channel_equalizer ) extends DspTester(c) {
////Tests are here 
//    var A=Complex(1.0,-1.0)
//    var ref=Seq.fill(64){Complex(pow(2,10)-1,pow(2,10)-1)}
//    poke(c.io.estimate_format,1)
//    poke(c.io.A,A)
//    poke(c.io.reference_in,ref) 
//    step(5)
//    fixTolLSBs.withValue(1) {
//        expect(c.io.Z, (A.conjugate/ref)*A)
//        expect(c.io.estimate_out, (A.conjugate)/ref)
//    }
//    step(1)
//    ref=Seq.fill(64){Complex(pow(2,15)-1,pow(2,15)-1)}
//    poke(c.io.reference_in,ref) 
//    poke(c.io.estimate_format,0)
//    step(5)
//    fixTolLSBs.withValue(1) {
//        expect(c.io.Z, (ref/A)*A)
//        expect(c.io.estimate_out, ref/A)
//    }
//
//}
//
////This is the test driver 
//object unit_test extends App {
//    iotesters.Driver.execute(args, () => new channel_equalizer(
//            n=16,
//            symbol_length=64,
//            users=16
//        )
//    ){
//            c=>new unit_tester(c)
//    }
//}

