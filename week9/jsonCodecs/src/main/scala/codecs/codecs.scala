package codecs


/**
 * A data type modeling JSON values.
 *
 * For example, the `42` integer JSON value can be modeled as `Json.Num(42)`
 * */

sealed trait Json {
  /**
   * Try to decode this JSON value into a value of type `A` by using
   * the implicit decoder.
   *
   * Note that you have to explicitly fix `A` type parameter when you call the method:
   *
   * {{{
   *   someJsonValue.decodeAs[User] // OK
   *   someJsonValue.decodeAs       // Wrong!
   * }}}
   */
  def decodeAs[A](implicit decoder: Decoder[A]): Option[A] = decoder.decode(this)
}

object Json {
  /** The JSON `null` value */
  case object Null extends Json

  /** The JSON boolean value */
  case class Bool(value: Boolean) extends Json

  /** The JSON numeric values */
  case class Num(value: BigDecimal) extends Json

  /** The JSON string values */
  case class Str(value: String) extends Json

  /** The JSON objects */
  case class Obj(fields: Map[String, Json]) extends Json

  /** The JSON arrays */
  case class Arr(items: List[Json]) extends Json
}

/**
 * A type class that turns a value of type `A` into its JSON representation
 *  */

trait Encoder[-A] {

  def encode(value: A): Json

  /**
   * Transform this `Encoder[A]` into an `Encoder[B]`, given a transformation function
   * from `B` to `A`
   *
   * For instance, given a `Encoder[String]`, we can get an `Encoder[UUID]`
   *
   * {{{
   *   def uuidEncoder(given StringEncoder: Encoder[String]): Encoder[UUID] =
   *     stringEncoder.transform[UUID](uuid => uuid.toString)
   * }}}
   *
   * This operation is also known as "contramap"
   * */
  def transform[B](f: B => A): Encoder[B] =
    Encoder.fromFunction[B](value => this.encode(f(value)))
}

object Encoder extends EncoderInstances {
  /**
   * Convenient method for creating an instance of encoder from a function `f`
   * */
//  def fromFunction[A](f: A => Json): Encoder[A] = new Encoder[A] {
//    def encode(value: A): Json = f(value)
//  }
  def fromFunction[A](f: A => Json): Encoder[A] = (value: A) => f(value)

}

trait EncoderInstances {

  /** An encoder for the `Unit` value */
  implicit val unitEncoder: Encoder[Unit] =
    Encoder.fromFunction(_ => Json.Null)

  /** An encoder for the `Int` values */
  implicit val intEncoder: Encoder[Int] =
    Encoder.fromFunction(n => Json.Num(n))

  /** An encoder for the `String` values */
  implicit val stringEncoder: Encoder[String] =
    Encoder.fromFunction(s => Json.Str(s))

  /** An encoder for the `Boolean` values */
  implicit val booleanEncoder: Encoder[Boolean] =
    Encoder.fromFunction(b => Json.Bool(b))

  /**
   * Encodes a list of values of type `A` into a JSON array containing
   * the list elements encoded with the given `encoder`
   *  */
  // note that it's `def` here instead of `val`, because it's parameterized function
  implicit def listEncoder[A](implicit encoder: Encoder[A]): Encoder[List[A]] =
    Encoder.fromFunction(as => Json.Arr(as.map(encoder.encode)))

}

/**
 * A specialization of `Encoder` that returns JSON objects only
 *  */
trait ObjectEncoder[-A] extends Encoder[A] {
  // Refines the encoding result to Json.Obj
  def encode(value: A): Json.Obj

  /**
   * Combines `this` encoder with `that` encoder
   * Returns an encoder producing a JSON object containing both
   * fields of `this` encoder and fields of `that` encoder
   * */
  def zip[B](that: ObjectEncoder[B]): ObjectEncoder[(A, B)] =
    ObjectEncoder.fromFunction { case(a,b) =>
      Json.Obj(this.encode(a).fields ++ that.encode(b).fields)
    }
}

object ObjectEncoder {
  /**
   * Convenient method for creating an instance of object encoder from a function `f`
   * */
//  def fromFunction[A](f: A => Json.Obj): ObjectEncoder[A] = new ObjectEncoder[A] {
//    override def encode(value: A): Json.Obj = f(value)
//  }
  def fromFunction[A](f: A => Json.Obj): ObjectEncoder[A] = (value: A) => f(value)

  /**
   * An encoder for values of type `A` that produces a JSON object with one field
   * named according to the supplied `name` and containing the encoded value
   * */
  def field[A](name: String)(implicit encoder: Encoder[A]): ObjectEncoder[A] =
    ObjectEncoder.fromFunction(a => Json.Obj(Map(name -> encoder.encode(a))))
}

/**
 * The dual of encoder. Decodes a serialized value into its initial type `A`
 * */
trait Decoder[+A] {
  /**
   * @param data The data to de-serialize
   * @return The decoded value wrapped in `Some`, or `None` if decoding failed
   * */
  def decode(data: Json): Option[A]

  /**
   * Combines `this` decoder with `that` decoder.
   * Returns a decoder that invokes both `this` decoder and `that`
   * decoder and returns a pair of decoded value in case both succeed,
   * or `None` if at least one failed.
   */
  def zip[B](that: Decoder[B]): Decoder[(A, B)] =
    Decoder.fromFunction(json => this.decode(json).zip(that.decode(json)))

  /**
   * Transforms this `Decoder[A]` into a `Decoder[B]`, given a transformation function
   * from `A` to `B`.
   *
   * This operation is also known as “map”.
   */
  def transform[B](f: A => B): Decoder[B] =
    Decoder.fromFunction[B](json => this.decode(json).map(f))
}

object Decoder extends DecoderInstances {

//  def fromFunction[A](f: Json => Option[A]): Decoder[A] = new Decoder[A] {
//    override def decode(data: Json): Option[A] = f(data)
//  }
  /**
   * Convenient method to build a decoder instance from a function `f`
   */
  def fromFunction[A](f: Json => Option[A]): Decoder[A] = (data: Json) => f(data)

  /**
   * Alternative method for creating decoder instances
   */
  def fromPartialFunction[A](pf: PartialFunction[Json, A]): Decoder[A] =
    fromFunction(pf.lift)

}

trait DecoderInstances {

  /** A decoder for the `Unit` value */
  implicit val unitDecoder: Decoder[Unit] =
    Decoder.fromPartialFunction{case Json.Null => ()}

  /** A decoder for `Int` values. */
  implicit val intDecoder: Decoder[Int] =
    Decoder.fromPartialFunction {
      case Json.Num(n) if n.isValidInt => n.intValue
    }

  /** A decoder for `String` values */
  implicit val stringDecoder: Decoder[String] =
    Decoder.fromPartialFunction {
      case Json.Str(s) => s
    }

  /** A decoder fro `Boolean` values */
  implicit val booleanDecoder: Decoder[Boolean] =
    Decoder.fromPartialFunction {
      case Json.Bool(b) => b
    }

  /**
   * A decoder for JSON arrays. It decodes each item of the array
   * using the given `decoder`. The resulting decoder succeeds only
   * if all the JSON array items are successfully decoded.
   */
  implicit def listDecoder[A](implicit decoder: Decoder[A]): Decoder[List[A]] =
    Decoder.fromPartialFunction(jsonAs => jsonAs.map())
}