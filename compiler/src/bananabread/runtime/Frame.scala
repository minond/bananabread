package bananabread
package runtime

import value.Value

import scala.collection.mutable.Map


class Frames:
  private var frame = Frame()

  def next = frame = frame.next
  def prev = frame = frame.prev
  def curr = frame
  def from(f: Frame) = frame = frame.from(f)


case class Frame(val env: Map[String, Value] = Map.empty, parent: Option[Frame] = None):
  def get(label: String): Option[Value] = (env.get(label), parent) match
    case (Some(v), _) => Some(v)
    case (_, Some(p)) =>  p.get(label)
    case _ => None

  def next = Frame(Map.empty, Some(this))
  def prev = parent.getOrElse(???)

  def from(frame: Frame) = Frame(frame.env, Some(this))
  def put(label: String, value: Value) = env.put(label, value)
