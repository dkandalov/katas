package ru.animalquiz

ant = new AntBuilder()
basePath = "/Users/dima/Library/Application Support/IntelliJIdea13/live-plugins/remote-vcsacces/"

def jarClasses() {
  def tempFolder = "${basePath}/out/core-jar"
  ant.mkdir(dir: "${tempFolder}/codemining/core")
  ant.copy(toDir: "${tempFolder}/codemining/core") {
    ant.fileset(dir: "${basePath}/src/main/groovy/codemining/core") { ant.exclude(name: "**/*.groovy") }
    ant.fileset(dir: "${basePath}/out/production/remote-vcsacces/codemining/core/")
  }
  ant.jar(baseDir: tempFolder, destFile: "${basePath}/code-mining-core.jar")
  ant.delete(dir: tempFolder)
}

def jarSources() {
  def tempFolder = "${basePath}/out/core-src-jar"
  ant.mkdir(dir: "${tempFolder}/codemining/core")
  ant.copy(toDir: "${tempFolder}/codemining/core") {
    ant.fileset(dir: "${basePath}/src/main/groovy/codemining/core")
    ant.fileset(dir: "${basePath}/src/test/groovy/codemining/core")
  }
  ant.jar(baseDir: tempFolder, destFile: "${basePath}/code-mining-core-src.jar")
  ant.delete(dir: tempFolder)
}


jarClasses()
jarSources()