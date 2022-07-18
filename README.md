# USAGE

```bash
# You can write any resolution(1080 1920 is my display resolution)
sbt assembly
java -jar target/scala-2.13/hello-assembly-0.1.0-SNAPSHOT.jar 1080 1920 > a.ppm

# OR 

cd src/main/scala
scalac -classpath . main
scala -J-Xmx4G -classpath . Main 1080 1920 > a.ppm
```
