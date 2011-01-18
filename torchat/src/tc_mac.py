# vim: set tw=0 sw=4 sts=4 expandtab:
import wx

def getActiveWindow():
    wins = wx.GetTopLevelWindows()
    for win in wins:
        if win.IsActive():
            return win
    return None

class App(wx.App):
    def __init__(self, *args, **kwargs):
        wx.App.__init__(self, *args, **kwargs)
        self.Bind(wx.EVT_ACTIVATE_APP, self.onActivate)

        fileMenu = wx.Menu()

        item = fileMenu.Append(wx.ID_EXIT)
        self.Bind(wx.EVT_MENU, self.onExit, item)

        item = fileMenu.Append(wx.ID_OPEN, text = 'Open Contacts')
        self.Bind(wx.EVT_MENU, self.onOpenContacts, item)

        item = fileMenu.Append(wx.ID_CLOSE, text = '&Close\tCtrl+W')
        self.Bind(wx.EVT_UPDATE_UI, self.canClose, item)
        self.Bind(wx.EVT_MENU, self.onClose, item)

        #item = fileMenu.Append(wx.ID_PREFERENCES, text = "Settings")
        #self.Bind(wx.EVT_MENU, self.onPrefs, item)

        editMenu = wx.Menu()

        item = editMenu.Append(wx.ID_CUT, text = 'Cut\tCtrl+X')
        self.Bind(wx.EVT_UPDATE_UI, self.canCut, item)
        self.Bind(wx.EVT_MENU, self.onCut, item)

        item = editMenu.Append(wx.ID_COPY, text = '&Copy\tCtrl+C')
        self.Bind(wx.EVT_UPDATE_UI, self.canCopy, item)
        self.Bind(wx.EVT_MENU, self.onCopy, item)

        item = editMenu.Append(wx.ID_PASTE, text = 'Paste\tCtrl+V')
        self.Bind(wx.EVT_UPDATE_UI, self.canPaste, item)
        self.Bind(wx.EVT_MENU, self.onPaste, item)

        mb = wx.MenuBar()
        mb.Append(fileMenu, "&File")
        mb.Append(editMenu, "&Edit")
        wx.MenuBar.MacSetCommonMenuBar(mb)


    def BringWindowToFront(self):
        try:
            mw = self.GetTopWindow()
            mw.Show(True)
            mw.Raise()
        except:
            pass

    def onActivate(self, event):
        if event.GetActive():
            self.BringWindowToFront()
        event.Skip()

    def onOpenContacts(self, event):
        self.BringWindowToFront()

    def canClose(self, event):
        # This isn't implemented for some reason...
        # win = wx.GetActiveWindow()
        # So cache this value
        self.win = getActiveWindow()
        event.Enable(self.win is not None)

    def onClose(self, event):
        self.win.Close()

    def MacReopenApp(self):
        self.BringWindowToFront()

    def onExit(self, event):
        self.GetTopWindow().exitProgram()

    def canCut(self, event):
        focus = wx.Window.FindFocus()
        # Annoyingly without the is True this returns None sometimes
        event.Enable((focus and hasattr(focus, 'CanCut') and focus.CanCut()) is True)

    def onCut(self, event):
        focus = wx.Window.FindFocus()
        focus.Cut()
        
    def canCopy(self, event):
        focus = wx.Window.FindFocus()
        event.Enable((focus and hasattr(focus, 'CanCopy') and focus.CanCopy()) is True)

    def onCopy(self, event):
        focus = wx.Window.FindFocus()
        focus.Copy()
        
    def canPaste(self, event):
        focus = wx.Window.FindFocus()
        event.Enable((focus and hasattr(focus, 'CanPaste') and focus.CanPaste()) is True)

    def onPaste(self, event):
        focus = wx.Window.FindFocus()
        focus.Paste()
