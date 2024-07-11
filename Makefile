check-docs:
	scala-cli compile README.md *.scala

test:
	scala-cli test *.scala
	scala-cli test --native *.scala
	scala-cli test --js *.scala

publish-snapshot:
	scala-cli config publish.credentials s01.oss.sonatype.org env:SONATYPE_USERNAME env:SONATYPE_PASSWORD
	scala-cli publish *.scala --signer none
	scala-cli publish *.scala --native --signer none
	scala-cli publish *.scala --js --signer none

publish:
	scala-cli config publish.credentials s01.oss.sonatype.org env:SONATYPE_USERNAME env:SONATYPE_PASSWORD
	./.github/workflows/import-gpg.sh
	scala-cli publish *.scala --signer gpg --gpg-key 15A7215B6CD4016A
	scala-cli publish *.scala --js --signer gpg --gpg-key 15A7215B6CD4016A
	scala-cli publish *.scala --native --signer gpg --gpg-key 15A7215B6CD4016A

code-check:
	scala-cli fmt . --check

run-example:
	scala-cli run README.md *.scala -M helloDecline -- --help

pre-ci:
	scala-cli fmt *.scala
