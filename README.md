## Development Environment for Apache Flex ##

Yes, you read it right Apache. The goal of project is to provide
high quality stable Flex development environment to Emacs users.

## Status ##

This code in on early stage, don't expect too much out of box.

## How To Build Your Project With FCSH ##

Put build.el file in root of your Flex project. It must contain
build target, that similar to code below:
`;; fcsh-mode compile settings
((compiler "mxmlc")
 (flags "-incremental=true -optimize=true -actionscript-file-encoding=UTF-8")
 (script "/absolute/pathTo/yourProject/Main.mxml"))`

than do M-x flashdev-fcsh-build
