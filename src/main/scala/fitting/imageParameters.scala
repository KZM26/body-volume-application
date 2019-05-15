package fitting

import scalismo.faces.color.RGBA
import scalismo.faces.image.PixelImageDomain
import scalismo.faces.parameters.ViewParameter
import scalismo.faces.render._
import scalismo.geometry.{Point, Point2D, Vector, _3D}

case class imageParameters(
                            camera: CameraParameters,
                            imageSize: imageSize,
                            view: ViewParameter
                         ) {
  def viewTransform: Affine3D = view.viewTransform
  def pointShader: PointShader = camera.projection.pointShader(viewTransform)
  def renderTransform: Point[_3D] => Point[_3D] = {p => imageSize.screenTransform(pointShader(p))}
}

object imageParameters {

  import CameraParameters.sensor35mm
  private def defCam(focalLength: Double) = CameraParameters(focalLength, Point2D.origin, sensor35mm, orthographic = false, near = 1, far = 10000e3)
  //private def for35mmFilmDF(focalLength: Double) = Camera(focalLength, Point2D.origin, sensor35mm, orthographic = false, near = 500, far = 500)
  /** parameters describing the view setup (camera transform) */
  val default: imageParameters = imageParameters(
    view = ViewParameter.away1m,
    camera = CameraParameters.for35mmFilm(100.0),
    imageSize = imageSize(640, 480)
  )
  val side: imageParameters = imageParameters(
    view = ViewParameter(Vector(1000f, 0f, 0f), 0f, 0.5f*Math.PI, 0f),
    camera = CameraParameters.for35mmFilm(1000.0),
    imageSize = imageSize(640, 480)
  )

  val medioLateral: imageParameters = imageParameters(
    view = ViewParameter(Vector(1000f, 0f, 0f), 0.5*Math.PI, 0f, -0.5*Math.PI),
    camera = defCam(350.0),
    imageSize = imageSize(640, 480)
  )

  val anteriorPosterior: imageParameters = imageParameters(
    view = ViewParameter(Vector(0f, 1000f, 0f), -0.5f*Math.PI, 0f, 0f),
    camera = defCam(350.0),
    imageSize = imageSize(640, 480)
  )
  val iniImage= imageParameters(
    view = ViewParameter(Vector(0f, 0f, 0f), 0.5f*Math.PI, 0f, 0f),
    camera = CameraParameters.for35mmFilm(100),
    imageSize = imageSize(640, 480)
  )

}

case class imageSize(width: Int, height: Int) {
  val domain = PixelImageDomain(width, height)

  def aspectRatio: Double = width.toDouble / height

  /** create an empty ZBuffer for rendering */
  def zBuffer(bgColor: RGBA): RenderBuffer[RGBA] = ZBuffer[RGBA](width, height, bgColor)

  def screenTransform = imageTransform(width, height)

  /** scale the image size, keeps aspect ratio */
  def scaleImage(scale: Double): imageSize = copy(
    width = (width * scale).toInt,
    height = (height * scale).toInt
  )
}


/** transform a point from normalized coordinates [-1,1]x[-1,1]x[-1,1] to window coordinates [0,w]x[0,h]x[near,far] (y still upwards!) used by renderer */
case class imageTransform(width: Int, height: Int, near: Double = 0.0, far: Double = 1.0) extends InvertibleTransform3D {
  override def apply(p: Point[_3D]): Point[_3D] = Point(
    width / 2.0 * (p.x + 1.0),
    height / 2.0 * (p.y + 1.0),
    (far - near) / 2.0 * p.z + (far + near) / 2.0
  )

  /** apply transform to a 3d vector */
  override def apply(v: Vector[_3D]): Vector[_3D] = Vector(
    width / 2.0 * (v.x + 1.0),
    height / 2.0 * (v.y + 1.0),
    (far - near) / 2.0 * v.z + (far + near) / 2.0
  )

  /** inverted version of this transform */
  override def inverted: InvertibleTransform3D = inverseImageTransform(width, height, near, far)
}


/** inverse window transform: from [0,width]x[0,height]x[near,far] to [-1,1]x[-1,1]x[-1,1] */
case class inverseImageTransform(width: Int, height: Int, near: Double = 0.0, far: Double = 1.0) extends InvertibleTransform3D {
  override def apply(x: Point[_3D]): Point[_3D] = Point(
    x.x * 2.0 / width - 1,
    x.y * 2.0 / height - 1,
    (x.z - (far + near) / 2) * 2.0 / (far - near)
  )

  /** apply transform to a 3d vector */
  override def apply(v: Vector[_3D]): Vector[_3D] = Vector(
    v.x * 2.0 / width - 1,
    v.y * 2.0 / height - 1,
    (v.z - (far + near) / 2) * 2.0 / (far - near)
  )

  override def inverted: InvertibleTransform3D = imageTransform(width, height, near, far)
}