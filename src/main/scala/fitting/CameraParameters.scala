package fitting

import scalismo.faces.render._
import scalismo.geometry._

/** camera parameterization */
case class CameraParameters(focalLength: Double,
                            principalPoint: Point[_2D],
                            sensorSize: Vector[_2D],
                            near: Double,
                            far: Double,
                            orthographic: Boolean) {

  require(focalLength > 0.0, "focal length must be positive")
  require(sensorSize.x > 0.0 && sensorSize.y > 0.0, "sensor size must be positive")
  require(near < far, "near plane must be closer than far plane")

  /** projection of this camera, ignores principalPoint, no offset (wrong units, PP is in pixels!) */
  def projection: FrustumProjection with Transform4x4 = {
    if (orthographic)
      FrustumOrthographicProjection(new Frustum(-sensorSize.x/2.0,sensorSize.x/2.0,-sensorSize.y/2.0,sensorSize.y/2.0,near,far).withCenter(principalPoint))
    else
      FrustumPinholeProjection(Frustum.fromFocalWithSensor(focalLength, sensorSize, near, far).withCenter(principalPoint))
  }

  /** viewing frustum of this scene */
  def frustum: Frustum = Frustum.fromFocalWithSensor(focalLength, sensorSize, near, far).withCenter(principalPoint)
}

object CameraParameters{
  val sensor35mm = Vector(36, 24)

  def for35mmFilm(focalLength: Double) = CameraParameters(focalLength, Point2D.origin, sensor35mm, orthographic = false, near = 10, far = 10000e3)
}