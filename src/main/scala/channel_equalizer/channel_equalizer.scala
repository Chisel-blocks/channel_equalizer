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
        val  estimate_in= Input(DspComplex(
                FixedPoint((2*n).W,(n).BP),
                FixedPoint((2*n).W,(n).BP)
            ) 
        )
        val  estimate_out= Output(DspComplex(
                FixedPoint((2*n).W,(n).BP),
                FixedPoint((2*n).W,(n).BP)
            ) 
        )

        val  estimate_format= Input(UInt(1.W))
        val reference_mem_write_enable=(Input(Bool()))
        val reference_mem_read_enable=(Input(Bool()))
        

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
    val reference_mem=SyncReadMem(scala.math.pow(2,log2Ceil(symbol_length)).toInt,io.estimate_out.cloneType)
    val estimate_mem=Seq.fill(users){SyncReadMem(scala.math.pow(2,log2Ceil(symbol_length)).toInt,io.estimate_out.cloneType)}

    

    val r_A=RegInit(0.U.asTypeOf(io.A.cloneType))
    val r_Z=RegInit(0.U.asTypeOf(reciprocal.Q.cloneType))
    val r_reference_in=RegInit(0.U.asTypeOf(io.reference_in.cloneType))
    val r_N=RegInit(0.U.asTypeOf(reciprocal.N.cloneType))
    val r_D=RegInit(0.U.asTypeOf(reciprocal.D.cloneType))
    val r_estimate_format=RegInit(0.U.asTypeOf(io.estimate_format))

    //Register assingments
    r_A:=io.A
    r_reference_in:=io.reference_in
    r_estimate_format:=io.estimate_format
    
    //Reciprocal connections
    reciprocal.N:=r_N
    reciprocal.D:=r_D
    r_Z:=reciprocal.Q
    when (r_estimate_format === 0.U) {
        // Reciprocal channel response.
        // Compensation required to get the reference
        // for local beamforming
        r_N:=r_reference_in.asTypeOf(reciprocal.D.cloneType)
        r_D:=r_A.asTypeOf(reciprocal.N.cloneType)
    }.elsewhen(r_estimate_format === 1.U) {
        // Channel response. 
        // Attenuation relative to reference
        r_N:=r_A.conj().asTypeOf(reciprocal.N.cloneType)
        r_D.real:=r_reference_in.asTypeOf(reciprocal.D.cloneType).real
        r_D.imag:=r_reference_in.asTypeOf(reciprocal.D.cloneType).imag
    }.otherwise {
        // Reciprocal format.
        // Compensation required to get the reference
        // for local beamforming
        r_N:=r_reference_in.asTypeOf(reciprocal.D.cloneType)
        r_D:=r_A.asTypeOf(reciprocal.N.cloneType)
    }

    io.estimate_out:=r_Z
    
    val w_Z=Wire(reciprocal.Q.cloneType)
    w_Z:=r_Z*r_A.asTypeOf(reciprocal.D.cloneType)
    io.Z.real:=(w_Z.real << n/2).round.asSInt
    io.Z.imag:=(w_Z.imag << n/2).round.asSInt
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
class unit_tester(c: channel_equalizer ) extends DspTester(c) {
//Tests are here 
    var A=Complex(1.0,-1.0)
    var ref=Complex(pow(2,10)-1,pow(2,10)-1)
    poke(c.io.estimate_format,1)
    poke(c.io.A,A)
    poke(c.io.reference_in,ref) 
    step(5)
    fixTolLSBs.withValue(1) {
        expect(c.io.Z, (A.conjugate/ref)*A)
        expect(c.io.estimate_out, (A.conjugate)/ref)
    }
    step(1)
    ref=Complex(pow(2,15)-1,pow(2,15)-1)
    poke(c.io.reference_in,ref) 
    poke(c.io.estimate_format,0)
    step(5)
    fixTolLSBs.withValue(1) {
        expect(c.io.Z, (ref/A)*A)
        expect(c.io.estimate_out, ref/A)
    }

}

//This is the test driver 
object unit_test extends App {
    iotesters.Driver.execute(args, () => new channel_equalizer(
            n=16,
            symbol_length=64,
            users=16
        )
    ){
            c=>new unit_tester(c)
    }
}

