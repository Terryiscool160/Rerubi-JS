# Rerubi-JS
Rerumu's [Rerubi](https://github.com/Rerumu/Rerubi) recreated in JS

unfinished, doesn't work as of now

you'll need to produce your own lua bytecode file, you can run `luac -o ./Out.out ./Test.lua` to do so with lua installed.
keep in mind that rerubi only supports 5.1 bytecode files in the first place

i only needed the `GetMeaning` function to parse chunks, protos and other information i required for one of my projects.
although since so much of the script functions the exact same as the original rerubi, while also considering
it took a bit of time to get what i needed working that i could've spent on other stuff, i've put it here and will complete the rest of rerubi's functionality, making it a nice little lua js vm in a box, or in case someone needs it for the same functionality i did
