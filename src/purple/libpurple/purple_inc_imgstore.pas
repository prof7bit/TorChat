{$ifdef interface_type}
  PPurpleStoredImage = ^TPurpleStoredImage;

  { TPurpleStoredImage }

  TPurpleStoredImage = object
    function GetData: Pointer;
    function GetSize: PtrUInt;
  end;
{$endif}

{$ifdef import_func}
function  purple_imgstore_get_data(img: PPurpleStoredImage): Pointer; cdecl; external LIBPURPLE;
function  purple_imgstore_get_size(img: PPurpleStoredImage): csize_t; cdecl; external LIBPURPLE;
{$endif}

{$ifdef implementation}
{ TPurpleStoredImage }

function TPurpleStoredImage.GetData: Pointer;
begin
  Result := purple_imgstore_get_data(@Self);
end;

function TPurpleStoredImage.GetSize: PtrUInt;
begin
  Result := purple_imgstore_get_size(@Self);
end;
{$endif}
