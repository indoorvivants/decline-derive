check-docs:
	scala-cli compile README.md *.scala

test:
	scala-cli test *.scala --cross

publish-snapshot:
	scala-cli config publish.credentials central.sonatype.com env:SONATYPE_USERNAME env:SONATYPE_PASSWORD
	scala-cli config publish.credentials ossrh-staging-api.central.sonatype.com env:SONATYPE_USERNAME env:SONATYPE_PASSWORD
	scala-cli publish *.scala --signer none --cross

publish:
	scala-cli config publish.credentials central.sonatype.com env:SONATYPE_USERNAME env:SONATYPE_PASSWORD
	scala-cli config publish.credentials ossrh-staging-api.central.sonatype.com env:SONATYPE_USERNAME env:SONATYPE_PASSWORD
	./.github/workflows/import-gpg.sh
	scala-cli publish *.scala --signer gpg --gpg-key 9D8EF0F74E5D78A3 --cross

publish-local:
	scala-cli publish local *.scala --signer none --cross

code-check:
	scala-cli fmt . --check

run-example:
	scala-cli run README.md *.scala -M helloDecline -- --help
	scala-cli run README.md *.scala -M helloDecline -- bootstrap --help
	scala-cli run README.md *.scala -M helloDecline -- index --help
	scala-cli run README.md *.scala -M helloDecline -- run --help

pre-ci:
	scala-cli fmt *.scala
