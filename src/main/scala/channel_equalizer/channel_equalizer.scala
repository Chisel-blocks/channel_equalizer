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
import edge_detector._
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
        val  reference_out= Output(DspComplex(
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

        
        val reference_addr=Input(UInt(log2Ceil(symbol_length).W))
        val reference_read_en=Input(Bool())
        val reference_write_en=Input(Bool())

        val estimate_addr=Input(UInt(log2Ceil(symbol_length).W))
        val estimate_read_en=Input(Bool())
        val estimate_write_en=Input(Bool())

        val estimate_format= Input(UInt(1.W)) // 0 Ref/Channel response
                                              // 1 Channel response/Ref

        val equalize_sync=Input(Bool()) //Rising edge resets equalization counters

        val estimate_sync=Input(Bool()) //Rising edge resets the estimation counters
        val estimate_user_index=Input(UInt(log2Ceil(users).W)) // User to estimate for
        

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
    val reciprocal_latency=6
    
    val reference_mem=Module( new memblock(proto=io.reference_in.cloneType, symbol_length)).io
    val estimate_mem=VecInit(Seq.fill(users){
            Module( new memblock(proto=io.estimate_in(0).cloneType, symbol_length)).io
        }
    )
    val mem_latency=4
    val r_estimate_sync=RegInit(false.B)
    val r_equalize_sync=RegInit(false.B)
    r_estimate_sync:=io.estimate_sync
    r_equalize_sync:=io.equalize_sync
    val reference_read_edge= Module(new edge_detector()).io 
    reference_read_edge.A:=io.reference_read_en 
    val reference_write_edge= Module(new edge_detector()).io 
    reference_write_edge.A:=io.reference_write_en 
    val estimate_read_edge= Module(new edge_detector()).io 
    estimate_read_edge.A:=io.estimate_read_en 
    val estimate_write_edge= Module(new edge_detector()).io 
    estimate_write_edge.A:=io.estimate_write_en 
    val equalize_sync_edge= Module(new edge_detector()).io 
    equalize_sync_edge.A:=r_equalize_sync
    val estimate_sync_edge= Module(new edge_detector()).io 
    estimate_sync_edge.A:=r_estimate_sync

    // Signal IO registers
    val r_A=RegInit(0.U.asTypeOf(io.A.cloneType))
    val r_equalized=RegInit(0.U.asTypeOf(reciprocal.Q.cloneType))
    val r_Z=RegInit(0.U.asTypeOf(io.Z))
    // Registers for reference IO
    val r_reference_in=RegInit(0.U.asTypeOf(io.reference_in.cloneType))
    val r_reference_write_val=RegInit(0.U.asTypeOf(io.reference_in.cloneType))
    val r_reference_addr=RegInit(0.U.asTypeOf(io.reference_addr.cloneType))
    val r_reference_write_en=RegInit(0.U.asTypeOf(io.reference_write_en.cloneType))
    val r_reference_read_en=RegInit(0.U.asTypeOf(io.reference_read_en.cloneType))
    val r_reference_read_val=RegInit(0.U.asTypeOf(io.reference_out.cloneType))

    // Registers for estimate IO
    val r_estimate_user_index=RegInit(0.U.asTypeOf(io.estimate_user_index.cloneType))
    val r_estimate_in=RegInit(VecInit(Seq.fill(users)(0.U.asTypeOf(io.estimate_in(0).cloneType))))
    val r_estimate_addr=RegInit(0.U.asTypeOf(io.estimate_addr.cloneType))
    val r_estimate_write_en=RegInit(0.U.asTypeOf(io.estimate_write_en.cloneType))
    val r_estimate_write_val=RegInit(VecInit(Seq.fill(users)(0.U.asTypeOf(io.estimate_out(0).cloneType))))
    val r_estimate_read_en=RegInit(0.U.asTypeOf(io.estimate_read_en.cloneType))
    val r_estimate_read_val=RegInit(VecInit(Seq.fill(users)(0.U.asTypeOf(io.estimate_out(0).cloneType))))

    val r_N=RegInit(0.U.asTypeOf(reciprocal.N.cloneType))
    val r_D=RegInit(0.U.asTypeOf(reciprocal.D.cloneType))
    val r_estimate_format=RegInit(0.U.asTypeOf(io.estimate_format))

    val r_estimate_done=RegInit(false.B)
    val r_estimate_done_flag=RegInit(false.B)
    val r_write_en=RegInit(false.B)

    //Register assignments
    r_A:=ShiftRegister(io.A,1) // delay to sync with estimate sync
    r_estimate_format:=io.estimate_format

    r_reference_in:=io.reference_in
    r_reference_write_val:=io.reference_in
    r_reference_write_en:=io.reference_write_en
    r_reference_addr:=io.reference_addr
    r_reference_read_en:=io.reference_read_en

    (r_estimate_in,io.estimate_in).zipped.map(_:=_)
    (r_estimate_write_val,io.estimate_in).zipped.map(_:=_)
    r_estimate_read_en:=io.estimate_read_en

    // Mem input defaults
    reference_mem.write_addr:=r_reference_addr
    reference_mem.read_addr:=r_reference_addr
    reference_mem.write_val:=r_reference_in
    reference_mem.write_en:=false.B

    estimate_mem.map(_.write_addr:=r_estimate_addr)
    estimate_mem.map(_.read_addr:=r_estimate_addr)
    (estimate_mem,r_estimate_in).zipped.map(_.write_val:=_)
    estimate_mem.map(_.write_en:=false.B)

   
    // State machine for channel estimation and equalization
    val equalizing:: estimating :: updating :: reading:: Nil = Enum(4)

    val state=RegInit(equalizing)  // The default state
    
    val estimate_address_counter=RegInit(0.U(log2Ceil(symbol_length).W))
    val equalize_address_counter=RegInit(0.U(log2Ceil(symbol_length).W))

    // This runs in background so it is only dependent on sync
    when( equalize_sync_edge.rising ){
        equalize_address_counter:=0.U
    }.otherwise{
        equalize_address_counter:=equalize_address_counter+1.U
    }

    // State machine
    when ( state === equalizing ){
        // State operation
        reference_mem.read_addr:=0.U.asTypeOf(reference_mem.read_addr)
        estimate_address_counter:=0.U
        reference_mem.write_en:=false.B
        estimate_mem.map(_.read_addr:=equalize_address_counter)
        estimate_mem.map(_.write_en:=false.B)

        // State transition
        when( reference_write_edge.rising || estimate_write_edge.rising  ) {
            state:=updating
        }.elsewhen ( reference_read_edge.rising || estimate_read_edge.rising) {
            state:=reading 
        }.elsewhen ( estimate_sync_edge.rising ) {
            state:=estimating
        }. otherwise {
            state:=equalizing
        }
    }.elsewhen( state === updating ) {
        estimate_address_counter:=0.U
        // State operation
        when ( r_reference_write_en ) {
            reference_mem.write_addr:=r_reference_addr
            reference_mem.write_val:=r_reference_write_val
            reference_mem.write_en:=true.B 
        }
        when ( r_estimate_write_en ) {
            estimate_mem.map(_.write_addr:=r_estimate_addr)
            (estimate_mem,r_estimate_write_val).zipped.map(_.write_val:=_)
            estimate_mem.map(_.write_en:=true.B)
        }
        // State transition
        when (( ! r_reference_write_en ) && (! r_estimate_write_en )) {
            state:=equalizing
        }.otherwise{
            state:=updating
        }
    }.elsewhen( state === reading ) {
        estimate_address_counter:=0.U
        // State operation
        when ( r_reference_read_en ) {
            reference_mem.read_addr:=r_reference_addr
            r_reference_read_val:= reference_mem.read_val
        }
        when ( r_estimate_read_en ) {
            estimate_mem.map(_.read_addr:=r_estimate_addr)
            (r_estimate_read_val,estimate_mem).zipped.map(_:= _.read_val)
        }
        // State transition
        when (( ! r_reference_read_en ) && (! r_estimate_read_en )) {
            state:=equalizing
        }.otherwise{
            state:=reading
        }
    }.elsewhen( state === estimating ) {
        reference_mem.read_addr:=estimate_address_counter
        reference_mem.write_en:=false.B
        estimate_mem.map(_.write_addr:=ShiftRegister(estimate_address_counter,reciprocal_latency))
        estimate_mem(r_estimate_user_index).write_val.real:=reciprocal.Q.real.asTypeOf(estimate_mem(0).write_val.real)
        estimate_mem(r_estimate_user_index).write_val.imag:=reciprocal.Q.imag.asTypeOf(estimate_mem(0).write_val.imag)
        // State transition
        when ( estimate_address_counter === (symbol_length-1).asUInt && ! r_estimate_done_flag ) {
            estimate_address_counter:=0.U
            r_write_en:=true.B
            r_estimate_done_flag:=true.B
        }.elsewhen( r_estimate_done_flag ) {
            estimate_address_counter:=0.U
            r_write_en:=false.B
        }.otherwise {
            estimate_address_counter:=estimate_address_counter+1.U
            r_write_en:=true.B
            r_estimate_done_flag:=false.B
        }
        estimate_mem(r_estimate_user_index).write_en:=ShiftRegister(r_write_en,reciprocal_latency-1)
        r_estimate_done:=ShiftRegister(r_estimate_done_flag,reciprocal_latency-1)
        // State transition
        when ( ! r_estimate_done ) {
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
        r_D:=ShiftRegister(r_A.asTypeOf(reciprocal.D.cloneType),mem_latency)
    }.elsewhen(r_estimate_format === 1.U) {
        // Channel response. 
        // Attenuation relative to reference
        r_N:=ShiftRegister(r_A.conj().asTypeOf(reciprocal.N.cloneType),mem_latency)
        r_D.real:=reference_mem.read_val.asTypeOf(reciprocal.D.cloneType).real
        r_D.imag:=reference_mem.read_val.asTypeOf(reciprocal.D.cloneType).imag
    }.otherwise {
        // Reciprocal format.
        // Compensation required to get the reference
        // for local beamforming
        r_N:=reference_mem.read_val.asTypeOf(reciprocal.N.cloneType)
        r_D:=ShiftRegister(r_A.asTypeOf(reciprocal.D.cloneType),mem_latency)
    }

    
    // Equalization and rounding
    r_equalized:=reciprocal.Q*r_A.asTypeOf(reciprocal.D.cloneType)
    r_Z.real:=(r_equalized.real << n/2).round.asSInt
    r_Z.imag:=(r_equalized.imag << n/2).round.asSInt

    // Output assignments
    io.reference_out:=r_reference_read_val.asTypeOf(io.reference_out)
    io.estimate_out:=r_estimate_read_val
  
    io.Z:=r_Z
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

