Haskell-DragonNaturallySpeaking
===============================

HTTP server, that listens for requests of natural language commands. The commands are "compiled" by the Haskell server into keyboard shortcuts for the current application. The commands are then executed by the Objective-C code, which presses some keys (including modifiers) and may click the mouse.

e.g. the commands come from DragonNaturallySpeaking and are sent to Emacs.


DataFlow
=========

sound waves from my mouth to the microphone

-> NatSpeak (i.e. Dragon NaturallySpeaking) does expensive proprietary magic on the audio

-> Microsoft's `SAPI` (i.e. speech API) upon `COM` (maybe?)

-> NatLink (i.e. a NatSpeak plug-in), in C++

-> a `gotResultsObject` callback method, of a `load(allResults = True)` grammar object, gets called with the text, in NatLink's python bindings

-> a `JSON` request, with words and rules, over host-only network, from guest to host

-> the Haskell server, which `parse`s the phrase to commands, and `compile`s commands to actions, and `execute`s the actions

-> call Objective-C with the FFI (i.e. in memory), via the `language-c-inline` package

-> return the current application, via `NSWorkspace`, called by Haskell's `compile`

-> make a quartz event, via `CGEventCreateKeyboardEvent` or `CGEventCreateMouseEvent`, then send the event via `CGEventPostToPSN` or `CGEventPost`, called by Haskell's `execute`

(this project only does the dataflow from the Haskell server to the OS X application.)


Requirements
============

as of Autumn 2014, my setup's versions are:

    $ sw_vers
    ProductName:	Mac OS X
    ProductVersion:	10.9.4
    BuildVersion:	13E28

    $ GHC --version
    The Glorious Glasgow Haskell Compilation System, version 7.8.3

    $ GCC --version
    Configured with: --prefix=/Applications/Xcode.app/Contents/Developer/usr --with-gxx-include-dir=/usr/include/c++/4.2.1
    Apple LLVM version 5.1 (clang-503.0.40) (based on LLVM 3.4svn)
    Target: x86_64-apple-darwin13.3.0
    Thread model: posix

may work on older or newer versions of the above, but untested.



Extensions
==========

I'd like to be able to define the grammar in one place. The Haskell code grammar on the host, and the NatLink BNF grammar on the guest, are two places. I'd also like the Haskell grammar to be static (i.e. each command is a variant), and for the grammar to be easily editable, so I may need to learn Template Haskell.

I'd like to be able to change keyboard shortcuts in one place. If you have to change the mapping from command to keyboard shortcut in Haskell, and also change the mapping from keyboard shortcut to command in the application (e.g. Emacs `global-set-key` or IntelliJ keybinding XML), that's two places.

I'd like to support applications with custom action implementations (e.g. some modern enlightened application listens for HTTP requests, and runs the relevant command, maybe responding with a status or some context).

Make the dataflow a nice D3 graph.

Once I switch to developing with Linux, I may be able to integrate with XMonad, for more speed (?) and more context.

it would be nice if the two calls, from Haskell to Objective-C, were atomic e.g. by passing the current application back in.

Generalize applications to any context (e.g. page in a tab in the browser, buffer in Emacs, present working directory in terminal). Now, I may need to "lift" contexts into applications, by copying and pasting Emacs.app into Terminal.app, or making Firefox.app my music player by exclusively having YouTube.


