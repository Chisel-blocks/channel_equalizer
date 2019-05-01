
// Dsp-block channel_equalizer
// Description here
// Inititally written by dsp-blocks initmodule.sh, 20190429
package channel_equalizer

import chisel3.experimental._
import chisel3._
import dsptools.{DspTester, DspTesterOptionsManager, DspTesterOptions}
import dsptools.numbers._
import breeze.math.Complex
import complex_reciprocal._

class channel_equalizer_io( n : Int)
    extends Bundle {
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

        override def cloneType = (new channel_equalizer_io(
                n=n
            )
        ).asInstanceOf[this.type]
   }

class channel_equalizer( n: Int) extends Module {
    val io = IO(new channel_equalizer_io( 
            n=n
        )
    )
    val reciprocal=Module( new complex_reciprocal(
            w=n,
            b=n/2
        )
    ).io
    val r_A=RegInit(0.U.asTypeOf(reciprocal.N.cloneType))
    val r_Z=RegInit(0.U.asTypeOf(reciprocal.Q.cloneType))
    val r_reference_in=RegInit(0.U.asTypeOf(reciprocal.D.cloneType))
    r_A:=io.A.asTypeOf(reciprocal.N.cloneType)
    r_reference_in:=io.reference_in.asTypeOf(reciprocal.D.cloneType)

    reciprocal.N:=r_reference_in
    reciprocal.D:=r_A
    r_Z:=reciprocal.Q
    io.estimate_out:=r_Z

    val w_Z=Wire(reciprocal.Q.cloneType)
    w_Z:=r_Z*r_A
    io.Z.real:=(w_Z.real << n/2).round.asSInt
    io.Z.imag:=(w_Z.imag << n/2).round.asSInt
}

//This gives you verilog
object channel_equalizer extends App {
    chisel3.Driver.execute(args, () => new channel_equalizer(
            n=16
        )
    )
}

//This is a simple unit tester for demonstration purposes
class unit_tester(c: channel_equalizer ) extends DspTester(c) {
//Tests are here 
    val A=Complex(1,-1.0)
    val ref=Complex(32767,32767.0)
    poke(c.io.A,A)
    poke(c.io.reference_in,ref) 
    step(10)
    fixTolLSBs.withValue(1) {
        expect(c.io.Z, ref)
        expect(c.io.estimate_out, ref/A)
    }
}

//This is the test driver 
object unit_test extends App {
    iotesters.Driver.execute(args, () => new channel_equalizer(
            n=16
        )
    ){
            c=>new unit_tester(c)
    }
}

