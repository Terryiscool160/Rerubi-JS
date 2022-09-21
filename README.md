# Rerubi-JS
Rerumu's [Rerubi](https://github.com/Rerumu/Rerubi) converted to JS

unfinished, doesn't work as of now

you'll need to produce your own lua bytecode file, you can run `luac -o ./Out.out ./Test.lua` to do so with lua installed
keep in mind that rerubi only supports 5.1 bytecode files in the first place

only needed the `GetMeaning` function to parse chunks, protos and other information i required for one of my projects
although since so much of the script functions the exact same as the original rerubi, while also considering
it took a stupid amount of time to get what i needed working, i've put it here and will finish it, making it a nice
little lua js vm in a box, or in case someone needs it in the same usecase i did