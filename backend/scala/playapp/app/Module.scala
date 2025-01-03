// Adapted from: https://github.com/playframework/play-samples/tree/3.0.x/play-scala-rest-api-example

import javax.inject._

import com.google.inject.AbstractModule
import net.codingwell.scalaguice.ScalaModule
import play.api.{Configuration, Environment}
import v1.user._

class Module(environment: Environment, configuration: Configuration)
    extends AbstractModule
    with ScalaModule {
        override def configure() = {
        bind[UserRepository].to[UserRepositoryImpl].in[Singleton]()
    }
}
