<h1 align="center"> 
  <img src="https://github.com/Hunter200165/LPatternMatch/blob/main/lpm-logo.png?raw=true" alt="drawing" height="100"/> 
</h1>

# LPatternMatch
Pattern match engine adapted to Object Pascal from Lua 5.3 source code 

Name LPatternMatch means Lua Pattern Match, which is what it really is - translated and adapted C code from Lua 5.3 [lstrlib.c](https://www.lua.org/source/5.3/lstrlib.c.html)

To see detailed information about project internals - consider visiting [project wiki](https://github.com/Hunter200165/LPatternMatch/wiki)

# Why use (and why don't use) *this* instead of regex engines

While regex (regular expression) engines are much more powerful than pattern matching engine, using latter gives you those pros:
- The code of pattern matching engine is *much more* simpler and compact, it means that usually footprint of this enigne will be really small (190kB on Win64 using unicode strings/56kB on Win64 using non-Unicode strings with static ranges);
- Pattern match is usually enough for tasks where regex is used, for example - URLs parsing, mask matching and so on. If you do not use complicated features from regex libraries - you might want to use simple pattern matching engine to avoid unnecessary size overhead;
- Only one unit with no external dependencies;
- Suitable if input data is not very big or patters are not extremely complex.

But there are also problematic sides of pattern matching approach:
- Pattern matching gets very slow when input data is large or when many suffixes (like `+`, `*`, `-`) are used (especially `*`); It is so because of naive implementation of those routines in the engine itself, but that is why it is so compact;
- Pattern matching engine is not regex engine, so it lacks many useful features that good regex engines contain, like: non-capturing groups, `|` (or) operator, optional groups, negative/positive-lookaheads/lookbehinds and others.

# WARNINGS
This unit is not properly tested. Whilst it *should* work as original unit (lstrlib.c) does, I cannot guarantee that I have not missed something or made any error.

So basically, you may use this engine, but if you find an error - please report it, so I can see if I really missed something.

Also this project lacks testing suites or something like that, so I can judge that unit works only by limited amount of testing cases

# Unicode support

LPatternMatch.Core.pas natively supports Unicode (UTF-16, native format of unicode of FPC and Delphi). 

Just to say - normal Unicode support was one of the main reasons LPatternMatch was made, as I got tired of tries to get regex engine work with unicode normally (I mean, it may be not the fault of library itself but the effect of `it does not work on my machine`, but I needed this with Unicode so I made engine with Unicode support).

Unicode is turned on by default, I suppose it might decrease speed of execution when working with non-Unicode data.

If you do not need Unicode support - just comment out the directive `{$Define UNICODE}` in the LPatternMatch.Core.pas. Also, if you need only English language characters (so basically only first 127 symbols of ASCII table) - you might want to turn on static ranges comparison (uncomment directive `{$Define StaticRanges}` in LPatternMatch.Core.pas), which decreases size of exe dramatically (mostly because of Character unit).

# Delphi compatibility

~~Well, I do not know if it is going to work with Delphi, as I really tested it only on FPC (version 3.3.1-r49634 \[2021/08/06\] for x86_64). Also I do not have Delphi compiler right now to test it out. If you are interested in this or want to help with this - you may test it out and mail me.
Code looks like normal Object Pascal source without any dependence on Delphi/FPC side, so it should work normally, I guess.~~

Finally got compiler on my machine so could test it out and fix incompatibilies; Now unit should work the same on fpc and delphi.

# Pattern syntax

Only brief explanation will be put here. For full pattern syntax specification please read [original article from PIL](https://www.lua.org/pil/20.2.html)

Escape charater is `%` by default, however it can be changed in the LPatternMatch.pas (it is constant `LPM_L_ESC`).

There is a set of special symbols that mean special things in the patterns (`( ) . % + - * ? [ ^ $`); If they are going to be used in the pattern by their actual value - they must be escaped (prefixed with an escape character).

- Groups are defined by `( )` brackets in the pattern string
- Character classes are defined by `[ ]` brackets in the pattern string
- Start of the string is defined by `^`
- End of the string is defined by `$`

There are also special character classes, which may be used to make patterns shorter:
- `.` matches any character (including newlines)
- `%a` matches letter
- `%c` matches control character
- `%d` matches digit
- `%l` matches lower case letter
- `%p` matches punctuation character
- `%s` matches whitespace character
- `%u` matches upper case letter
- `%w` matches alphanumeric character (any letter or digit or underscope (`_`))
- `%x` matches hexadecimal digit (0123456789abcdefABCDEF)
- `%z` matches null character (0)

Special character class may be inverted by placing uppercase letter instead of lowercase (for example, `%D` will match anything but digit).

And also there are suffixes:
- `+` matches 1 or more repetitions of given class (greedy)
- `*` matches 0 or more repetitions of given class (greedy)
- `-` matches 0 or more repetitions of given class (non-greedy)
- `?` matches either 0 or 1 repetitions of given class

So for example `%d+` will try to match as many digits as possible but at least one.

# How to compile LPatternMatch on FPC

You may just use `ppcx64 LPatternMatch.pas` (replace `ppcx64` with any other name of compiler on other platforms) if it is in your PATH (on windows).

LPatternMatch can also be compiled using Lazarus (if you do not have compiler in your path or just do not prefer using cmd compilation).

# How to compile LPatternMatch on Delphi

You may just use `dcc64 LPatternMatch.pas` (replace `dcc64` with any other name of compiler on your platform/arch) if it is in your PATH (should be if you installed RAD Studio).

LPatternMatch can also be compiled using RAD Studio.

# Brief docs on how to use

P.S. Always check project's wiki to see code examples - presented here code snipped might not be updated for long time (and so now it is already out of date)

```pas

uses
    HPatternMatch.Core;

var LPM: TLPMMatchState;
    A, B: UnicodeString;
    Res: TLPMResult;
    i: Integer;
begin 
    { Define A and B, Source string and pattern string }
    A := 'hello world, hell no, hello my peaceful world';
    B := '(hello.-)%p.-(hello.+)';
    { Find string (or don't) }
    Res := LPM.Find(A, B, 1);
    { Check if there was an error 
        Basically it is the same as: if not (Res = 0) then
    }
    if LongBool(Res) then 
        WriteLn('There was an error matching string : ', Res)
    { Check if there is a match }
    else if LPM.Level = 0 then 
        WriteLn('No match found')
    else 
        for i := 0 to LPM.Level - 1 do begin 
            { Do whatever you want with results of matching. It is *that* simple }
            WriteLn('Capture[', i, ']: Pos = ', LPM.Capture[i].Position, '; Length = ', LPM.Capture[i].Length, '; Content = ', LPM.GetCaptureContent(i));
        end;
    { No need to free the instance, as it is record type }
end.
```

`LPatternMatch.pas` file contains interactive demo using this code (you can input source string and pattern from keyboard and to see the result of execution)

`input.txt` contains Unicode test cases (in case you cannot input unicode data from cmd). So to use this file you would want to start application like `LPatternMatch.exe <input.txt`

Please notice that this repo is created when I just created making simple examples, so the code is not properly documented; It will fixed in next releases

# Possible usage in other language

Although all the processing is done using Object Pascal, it is easily possible to turn LPatternMatch into library with C interface (so it means universal), as the code does not use any Object Pascal bound data types and mechanisms.

Native cdecl functions will be presented a bit later, when code will be better documented and maintained.

# Reference

Original code is not made by me; It is all done by Lua Team.

Lua is great scripting programming language I have been used for years, and when I thought about light and simple pattern matching engine I just remembered this wonderful language.

Official website: [Lua](https://www.lua.org)

Logo of language: ![Logo](http://www.andreas-rozek.de/Lua/Lua-Logo_64x64.png "Lua logo")

# Contacts

If you find a bug - please consider opening issue on github.

If you have a question - you may also open a discussion on github (it is lighter version of issues, so you can just ask your questions here) in real time, as chat.

If you want to contact me - you may use email (hunter200165@gmail.com), or discord (HFSoftware server, link is in my profile)

