         ***** General Information *****
         ===============================

The executable mmsyn7h is auxiliary for the
[mmsyn7ukr package](https://hackage.haskell.org/package/mmsyn7ukr). 
It works similarly to the
[mmsyn6ukr executable](https://hackage.haskell.org/package/mmsyn6ukr),
but uses the voice sound files in the current directory. This allows to change the 
needed files and to produce them by some other means (e. g. by 
mmsyn7ukr).
Besides, you can remove the created by mmsyn7ukr and mmsyn7h files
while the last one is running.

         ***** Processing *****
         ======================

The program mmsyn7h plays the newly created file once. Then it can delete 
the sound files in the current directory while being executed. To define
whether to clean the sound files after executing, user is prompted after
spcifying the input text.

If you enter as a first command line argument "-h", then the program only
prints informational message. If you specify as a first command line
argument "-v", then the program only prints its version number.

If you specify something else, the first command line argument
is being treated as a name for the resulting file voiced. The second
command line argument (if any) must be the controlling parameter
(for more information, refer to:
[mmsyn6ukr:genControl](https://hackage.haskell.org/package/mmsyn6ukr-0.8.0.0/docs/UkrainianLControl.html#v:genControl). 
If you specify further command line arguments as a Ukrainian text,
that contains only those sounds, which sound representations are
in the current directory (you can create them by e. g. mmsyn7ukr and mmsyn7l
programs in the same name packages), then the program will use only
these sounds representations additionally to the default ones
"-.wav", "0.wav" and "1.wav" and produce the sounding for the text.
For more information, refer to:
[mmsyn7ukr](https://hackage.haskell.org/package/mmsyn7ukr)
[mmsyn7s](https://hackage.haskell.org/package/mmsyn7s)
[mmsyn7l](https://hackage.haskell.org/package/mmsyn7l)
In such a case the program will not delete the created resulting 
sound files.

If you do not specify the Ukrainian text as the third and the next command line
arguments, then the resulting file will be played just after it is created
by the program. To remove all the created sound files from the directory,
please, specify the first character in the line when being promted to define
whether to remove sound files as "y". Otherwise, the program will not remove
any sound files while being executed.
