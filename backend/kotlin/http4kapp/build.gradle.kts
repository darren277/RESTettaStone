plugins {
    kotlin("jvm") version "1.8.22"
    application
}

application {
    mainClass.set("com.http4kapp.AppKt")
}

repositories {
    mavenCentral()
}

dependencies {
    implementation(platform("org.http4k:http4k-bom:5.44.0.0"))

    implementation("org.http4k:http4k-core")
    implementation("org.http4k:http4k-server-undertow")
    implementation("org.http4k:http4k-format-jackson")

    implementation("org.jetbrains.exposed:exposed-core:0.41.1")
    implementation("org.jetbrains.exposed:exposed-dao:0.41.1")
    implementation("org.jetbrains.exposed:exposed-jdbc:0.41.1")
    implementation("org.postgresql:postgresql:42.6.0")

    implementation("ch.qos.logback:logback-classic:1.4.7")

    // For reading from application.conf (Typesafe Config)
    implementation("com.typesafe:config:1.4.2")
}

tasks.jar {
    manifest {
        attributes["Main-Class"] = "com.http4kapp.AppKt"
    }

    from(configurations.runtimeClasspath.get().map { if (it.isDirectory) it else zipTree(it) })
    duplicatesStrategy = DuplicatesStrategy.EXCLUDE
}
