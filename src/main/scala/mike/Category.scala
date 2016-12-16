package mike

object Category extends GenericCategory[Function] {
  def id[A]: A => A = a => a
  def compose[A, B, C](g: B => C, f: A => B): A => C =
    g compose f // This is Function.compose, not a recursive call!
}
