unit purple_common;

{$mode objfpc}{$H+}

interface
const
  {$ifdef windows}
    LIBPURPLE = 'libpurple.dll';
  {$else}
    LIBPURPLE = 'purple';
  {$endif}


implementation

end.

