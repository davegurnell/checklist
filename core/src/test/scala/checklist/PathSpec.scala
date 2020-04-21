package checklist

import org.scalatest._
import org.scalatest.freespec._
import org.scalatest.matchers.should._

class PathSpec extends AnyFreeSpec with Matchers {
  "path.pathString" - {
    "empty path" in {
      PNil.pathString should be("")
    }

    "single field" in {
      ("field" :: PNil).pathString should be("field")
    }

    "nested fields" in {
      ("foo" :: "bar" :: PNil).pathString should be("foo/bar")
    }

    "single index" in {
      (123 :: PNil).pathString should be("123")
    }

    "nested indices" in {
      (123 :: 456 :: PNil).pathString should be("123/456")
    }

    "interleaved fields and indices" in {
      ("a" :: "b" :: 3 :: "c" :: 4 :: 5 :: "d" :: PNil).pathString should be(
        "a/b/3/c/4/5/d"
      )
    }
  }
}
