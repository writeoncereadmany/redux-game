stack --work-dir .stack-work-profile --profile build
stack --work-dir .stack-work-profile exec Redux-exe $1 -- +RTS  -p
