package crypto_ips

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec

class MontgomeryMulSpec extends AnyFreeSpec with ChiselScalatestTester {
  "Montgomery Multiplier should multiply" /* taggedAs RequiresVerilator */ in {
    test(new MontgomeryMul(256)).withAnnotations(Seq(/* VerilatorBackendAnnotation,  */WriteVcdAnnotation)) { dut =>
      dut.clock.step(3)
      val x = 202
      val y = 236
      val m = 239
      implicit val big_m = BigInt(m)
      val r = 256
      val rXr = (r * r) % m

      def getMP(x: BigInt, y: BigInt)(implicit m: BigInt): BigInt = {
        dut.clock.step()
        dut.io.din.bits.m.poke(m)
        dut.io.din.bits.x.poke(x)
        dut.io.din.bits.y.poke(y)
        dut.io.din.bits.k.poke(8) // FIXME!
        dut.io.din.valid.poke(true)
        dut.clock.step()
        dut.io.din.valid.poke(false)
        while(dut.io.dout.valid.peek().litToBoolean == false) {
          dut.clock.step()
        }
        dut.io.dout.bits.peekInt()
      }

      val mp_x = getMP(x, rXr)
      val mp_y = getMP(y, rXr)
      val mp_xy = getMP(mp_x, mp_y)
      val xy = getMP(mp_xy, 1)

      if(xy == (x * y) % m) 
        println(s"TEST PASS | xy mod m = $xy")
      else
        println(s"TEST FAILD | hardware: $xy, result: ${(x * y) % m}")
      // println(s"getMP: ${getMP(x, y)}")
    }
  }
}