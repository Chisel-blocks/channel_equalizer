
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

class channel_equalizer_io(w: Int, b:Int, weightbits : Int)
    extends Bundle {
        val A = Input(DspComplex(
                FixedPoint(w.W,b.BP),
                FixedPoint(w.W,b.BP)
            ) 
        )
        val Z = Output(DspComplex(
                FixedPoint(w.W,b.BP),
                FixedPoint(w.W,b.BP)
            ) 
        )
        val  write_reference_in= Input(DspComplex(
                SInt(weightbits.W),
                SInt(weightbits.W)
            ) 
        )
        val  read_reference_out= Output(DspComplex(
                SInt(weightbits.W),
                SInt(weightbits.W)
            ) 
        )

        override def cloneType = (new channel_equalizer_io(
                w=w, 
                b=b, 
                weightbits=weightbits
            )
        ).asInstanceOf[this.type]
   }

class channel_equalizer( w: Int, b:Int, weightbits : Int) extends Module {
    val io = IO(new channel_equalizer_io( 
            w=w, 
            b=b, 
            weightbits=weightbits
        )
    )
    val r_A=RegInit(0.U.asTypeOf(io.A))
    val r_Z=RegInit(0.U.asTypeOf(io.Z))
    io.read_reference_out:=0.U.asTypeOf(io.read_reference_out)
    r_A:=io.A
    r_Z:=r_A
    io.Z:=r_Z
}

//This gives you verilog
object channel_equalizer extends App {
    chisel3.Driver.execute(args, () => new channel_equalizer(
            w=16,
            b=8,
            weightbits=10
        )
    )
}

//This is a simple unit tester for demonstration purposes
class unit_tester(c: channel_equalizer ) extends DspTester(c) {
//Tests are here 
    poke(c.io.A.real, 5)
    poke(c.io.A.imag, 102)
    step(5)
    fixTolLSBs.withValue(0) {
        expect(c.io.Z.real, 5)
        expect(c.io.Z.imag, 102)
    }
}

//This is the test driver 
object unit_test extends App {
    iotesters.Driver.execute(args, () => new channel_equalizer(
            w=16,
            b=8,
            weightbits=10
        )
    ){
            c=>new unit_tester(c)
    }
}

