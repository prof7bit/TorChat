##############################################################################
#                                                                            #
# Copyright (c) 2007-2008 Bernd Kreuss <prof7bit@gmail.com>                  #
#                                                                            #
# This program is licensed under the GNU General Public License V3,          #
# the full source code is included in the binary distribution.               #
#                                                                            #
# Included in the distribution are files from other open source projects:    #
# - TOR Onion Router (c) The Tor Project, 3-clause-BSD                       #
# - SocksiPy (c) Dan Haim, BSD Style License                                 #
# - Gajim buddy status icons (c) The Gajim Team, GNU GPL                     #
#                                                                            #
##############################################################################

# Here are some classes that help me with some boring layout stuff and some
# other things with controls and their labels  inside sizers on a wx.Panel

import wx
import config


class Panel(wx.Panel):
    def __init__(self, parent):
        wx.Panel.__init__(self, parent, wx.ID_ANY)
        self.parent = parent
        self.px = 0
        self.py = 0
        self.controls = []
        self.last_separator = None
        self.outer_sizer = wx.BoxSizer()
        self.grid_sizer = wx.GridBagSizer(vgap=5, hgap=5)
        self.SetSizer(self.outer_sizer)
        self.outer_sizer.Add(self.grid_sizer, 1, wx.ALL | wx.EXPAND, 5)
        
    def addItem(self, item, offset_x):
        x = self.px + offset_x
        y = self.py
        self.grid_sizer.Add(item, (y, x))

    def addSeparatorItem(self, sep):
        self.grid_sizer.Add(sep.hbox, (self.py, self.px), (1, 2), wx.EXPAND)
        self.last_separator = sep
    
    def registerControl(self, control):
        self.controls.append(control)
    
    def newLine(self):
        self.py += 1
        
    def fit(self):
        self.outer_sizer.Fit(self.parent)
        
    def saveAllData(self):
        for control in self.controls:
            control.save()
        
        
class Control(object):
    def __init__(self, panel, label, default):
        self.panel = panel
        self.label = label
        self.from_config = False
        self.default = self.getDefault(default)
        self.wx_label = wx.StaticText(self.panel, wx.ID_ANY, self.label)
        self.wx_ctrl = None
        self.panel.registerControl(self)
    
        
    def getDefault(self, default):
        if type(default) == tuple:
            self.config_section = default[0]
            self.config_option = default[1]
            self.from_config = True
            return self.readConfig(default[0], default[1])
        else:
            return default
        
    def addToPanel(self):
        self.panel.addItem(self.wx_label, 0)
        if self.wx_ctrl:
            self.panel.addItem(self.wx_ctrl, 1)
        self.panel.newLine()
        try:
            self.panel.last_separator.controls.append(self)
        except:
            pass
    
    def setEnabled(self, enabled):
        self.wx_label.Enable(enabled)
        if self.wx_ctrl:
            self.wx_ctrl.Enable(enabled)
            
    def getValue(self):
        if self.wx_ctrl:
            return self.wx_ctrl.GetValue()
        else:
            return None
            
    def save(self):
        if self.from_config:
            config.set(self.config_section, self.config_option, self.getValue())

    def readConfig(self, section, option):
        return config.get(section, option)
                
                
class Text(Control):
    def __init__(self, panel, label, default, width=0):
        Control.__init__(self, panel, label, default)
        self.wx_ctrl = wx.TextCtrl(self.panel, wx.ID_ANY, self.default)
        if width:
            self.wx_ctrl.SetMinSize((width, -1))
        self.addToPanel()
    
        
class Check(Control):
    def __init__(self, panel, label, default, label2=""):
        Control.__init__(self, panel, label, default)
        self.wx_ctrl = wx.CheckBox(self.panel, wx.ID_ANY)
        self.wx_ctrl.SetValue(bool(self.default))
        self.wx_ctrl.SetLabel(label2)
        self.addToPanel()

    def readConfig(self, section, option):
        return config.getint(section, option)

class Separator(Control):
    def __init__(self, panel, label):
        #completely different constructor
        self.panel = panel
        self.hbox = wx.BoxSizer(wx.HORIZONTAL)
        
        self.wx_label = wx.StaticText(self.panel, wx.ID_ANY, label)
        self.hbox.Add(self.wx_label, 0, wx.EXPAND | wx.TOP, 5)
        
        #line must be in a box sizer or it will expand vertically
        sepbox = wx.BoxSizer(wx.VERTICAL)
        self.wx_ctrl = wx.StaticLine(self.panel, wx.ID_ANY)
        sepbox.Add(self.wx_ctrl, 0, wx.EXPAND | wx.TOP, 15)
        self.hbox.Add(sepbox, 1, wx.EXPAND)
        
        self.panel.addSeparatorItem(self)
        self.panel.newLine()
    
        #make the label bold
        font = self.wx_label.GetFont()
        font.SetWeight(wx.BOLD)
        self.wx_label.SetFont(font)
        
        #an array of controls below this separator
        self.controls = []
        
    def setEnabled(self, enabled):
        Control.setEnabled(self, enabled)
        for control in self.controls:
            control.setEnabled(enabled)
