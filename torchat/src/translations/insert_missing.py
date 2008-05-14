#!/usr/bin/python

import os
import sys

msg = """This little Script will insert the missing strings into
all incomplete translation files. It will create backup 
files with _ at the beginning.

If for example lang_en.py has the definition 

FOO_BAR = u"foo bar"

and that definition is missing in lang_de.py because 
lang_de.py is older then it will insert the missing 
definition into lang_de.py but it will be commented 
out, so that you can better find it:

# FOO_BAR = u"foo bar"

As a translator you now just open your language file, 
find all such missing translations, remove the # and 
translate them. 

Continue? (y/n) """

c = raw_input(msg)
if c != "y":
    sys.exit()

def readFile(filename):
    lines1 = []
    lines = open(filename).readlines()
    #combine lines with \ at the end
    not_at_end = False
    for line in lines:
        line=line.rstrip()
        if not_at_end:
            last = len(lines1)-1
            lines1[last] = lines1[last] + "\n" + line
        else:
            lines1.append(line)
            
        if line [-1:] == "\\":
            not_at_end = True
        else:
            not_at_end = False
            
    lines2 = []
    #combine multiline strings literals with """
    in_string = False
    for line in lines1:
        if in_string:
            last = len(lines2)-1
            lines2[last] = lines2[last] + "\n" + line
        else:
            lines2.append(line)
        
        if ' = u"""' in line:
            in_string = True
            
        if ' = """' in line:
            in_string = True

        #the closing """ must be at the end of the line
        if line[-3:] == '"""':
            in_string = False
            
    return lines2
        

def updateTranslation(filename):
    print "*** updating %s" % filename
    cnt = 0
    lang_xx_new = []
    lang_xx = readFile(filename)
    for line_en in lang_en:
        def_en = line_en.split(" = ")[0]
        found = False
        for line_xx in lang_xx:
            def_xx = line_xx.split(" = ")[0]
            if def_xx == def_en:
                found = True
                break
            
            if def_xx == "# " + def_en or def_xx == "#" + def_en:
                #also find already inserted and commented out definitions
                found = True
                break
        
        if not found:
            # no translation found, use the version 
            # from lang_en and insert a # before
            lang_xx_new.append("# " + line_en)
            print "inserting %s" % def_en
            cnt += 1
        else:
            # use the already translated version
            lang_xx_new.append(line_xx)

        if cnt:
            os.rename(filename, "_" + filename)
            f = open(filename, "w")
            f.write("\n".join(lang_xx_new))
            f.close

#load the english reference file            
lang_en = readFile("lang_en.py")

#update all files in current directory
filenames = os.listdir(".")
for filename in filenames:
    if filename[:5] == "lang_" and filename[-3:] == ".py" and filename != "lang_en.py":
        updateTranslation(filename)
