package lms.util

import java.util.Base64
import java.io.{Externalizable, ObjectInput, ObjectOutput}

trait ClosureCompare extends Externalizable {
  def writeExternal(x: ObjectOutput): Unit = {}

  def readExternal(x: ObjectInput): Unit = {
    throw new java.io.NotSerializableException("this is just a mock-up!")
  }

  def canonicalize(f: Serializable): String = {
    val s = new java.io.ByteArrayOutputStream()
    val o = new java.io.ObjectOutputStream(s)
    o.writeObject(f)
    Base64.getEncoder.encodeToString(s.toByteArray)
  }
}
