java -jar javacc.jar -OUTPUT_DIRECTORY=parser Clojure.jj
mkdir -p target
javac parser/*.java -d target/
