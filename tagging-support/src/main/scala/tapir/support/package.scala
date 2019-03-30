package tapir.support

import com.softwaremill.tagging.{@@, Tagger}
import tapir.Codec.PlainCodec
import tapir.{Codec, MediaType, SchemaFor}

package object tagging {
  implicit def taggedPlainCodec[U, T](implicit uc: PlainCodec[U], sf: SchemaFor[U @@ T]): Codec[U @@ T, MediaType.TextPlain, String] =
    uc.map(_.taggedWith[T])(identity).schema(sf.schema)
}
