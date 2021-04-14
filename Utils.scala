package sourdough
package utils


trait Print(inner: String = ""):
  self =>
    override def toString =
      if inner == ""
      then getClass.getSimpleName.toUpperCase
      else inner
