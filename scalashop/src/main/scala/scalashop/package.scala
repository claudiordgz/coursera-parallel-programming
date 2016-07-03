
import common._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    if(radius == 0) return src(x, y)
    def clampWidth(x: Int): Int = clamp(x, 0, src.width - 1)
    def clampHeight(y: Int): Int = clamp(y, 0, src.height - 1)
    val x1: Int = clampWidth(x - radius); val x2: Int = clampWidth(x + radius)
    val y1: Int = clampHeight(y - radius); val y2: Int = clampHeight(y + radius)
    val N = ((x2 - x1) + 1) * ((y2 - y1) + 1)
    var i = x1; var j = y1
    var r = 0; var g = 0; var b = 0; var a = 0
    while(i <= x2) {
      while(j <= y2) {
        val rgba: RGBA = src(i, j)
        r += red(rgba)
        g += green(rgba)
        b += blue(rgba)
        a += alpha(rgba)
        j += 1
      }
      j = y1
      i += 1
    }
    r = r / N; g = g / N; b = b / N; a = a / N
    rgba(r, g, b, a)
  }
}
