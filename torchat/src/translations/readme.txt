translations
============

lang_en.py is the only language file that is *guaranteed* to
be complete. If a language file misses a string, TorChat
will just use the version in lang_en.py 

If you have a new translation file then add it to this
directory and update the imports statements in __init__.py.

Translation files in the script directory take precedence over
the built-in ones in this folder. This is because if someone 
is using the portable windows version (torchat.exe) and just
wants to help translating he has no access to this folder.

So if you want to start a new translation, do the following:

- make a copy of lang_en.py (it is always complete) rename 
  it to lang_xx.py where xx is the language code used in 
  torchat.ini. and place it *outside* this folder, next to 
  torchat.exe or torchat.py

- change the language code in torchat.ini to the new one

- start translating the strings in your new lang_xx.py

- start TorChat with log_level 2 and look into the log file
  for error messages relating to the translation.

- put your name into the header of the file and send it
  to me, I will include it in the next release.

If you want to update an existing  translation because it is 
outdated and missing some strings:

- run TorChat with loglevel 2 to get a list of the missing strings.
  (if the file has been processed by insert_missing.py as it should
  be with files from regular releases, then there will be already all
  missing strings inserted but commented out in the file)

- Begin with a copy of the language file in question, add the missing 
  strings (just remove the # before the untranslated definition and 
  translate) and watch the output of the log to see when you are done. 
  
- Add your name to the header of the file, to receive fame and immortality, 
  send it to me and I will include it in the next release.

You can always work with a copy of the lang_xx file outside of
this folder, language files in the script directory take precedence
over the built-in ones in the folder "translations"

UNICODE!
========

- The file must be saved with the encoding UTF-8
- all string literals have to be in unicode notation:
  
  * wrong:
    FOO_BAR = "something"
  * right:
    FOO_BAR = u"something"
    
  