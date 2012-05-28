unit fastcgi_base;

interface

{
  Automatically converted by H2Pas 0.99.16 from fastcgi.h
  The following command line parameters were used:
    fastcgi.h
}

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


{
 * Listening socket file number
}

const
   FCGI_LISTENSOCK_FILENO = 0;     

type

   PFCGI_Header = ^FCGI_Header;
   FCGI_Header = record
      version : byte;
      reqtype : byte;
      requestIdB1 : byte;
      requestIdB0 : byte;
      contentLengthB1 : byte;
      contentLengthB0 : byte;
      paddingLength : byte;
      reserved : byte;
   end;
{
 * Number of bytes in a FCGI_Header.  Future versions of the protocol
 * will not reduce this number.
}

const
   FCGI_HEADER_LEN = 8;     

{
 * Value for version component of FCGI_Header
}
   FCGI_VERSION_1 = 1;     

{
 * Values for type component of FCGI_Header
}
   FCGI_BEGIN_REQUEST = 1;     
   FCGI_ABORT_REQUEST = 2;     
   FCGI_END_REQUEST = 3;     
   FCGI_PARAMS = 4;     
   FCGI_STDIN = 5;     
   FCGI_STDOUT = 6;     
   FCGI_STDERR = 7;     
   FCGI_DATA = 8;     
   FCGI_GET_VALUES = 9;     
   FCGI_GET_VALUES_RESULT = 10;     
   FCGI_UNKNOWN_TYPE = 11;     
   FCGI_MAXTYPE = FCGI_UNKNOWN_TYPE;     
   
{
 * Value for requestId component of FCGI_Header
}
   FCGI_NULL_REQUEST_ID = 0;     

type
   FCGI_BeginRequestBody = record
      roleB1 : byte;
      roleB0 : byte;
      flags : byte;
      reserved : array[0..4] of byte;
   end;

   FCGI_BeginRequestRecord = record
      header : FCGI_Header;
      body : FCGI_BeginRequestBody;
   end;
   
{
 * Mask for flags component of FCGI_BeginRequestBody
}

const
   FCGI_KEEP_CONN = 1;     
   
{
 * Values for role component of FCGI_BeginRequestBody
}

   FCGI_RESPONDER = 1;     
   FCGI_AUTHORIZER = 2;     
   FCGI_FILTER = 3;     

type

   FCGI_EndRequestBody = record
      appStatusB3 : byte;
      appStatusB2 : byte;
      appStatusB1 : byte;
      appStatusB0 : byte;
      protocolStatus : byte;
      reserved : array[0..2] of byte;
   end;

   FCGI_EndRequestRecord = record
      header : FCGI_Header;
      body : FCGI_EndRequestBody;
   end;
   
{
 * Values for protocolStatus component of FCGI_EndRequestBody
}

const
   FCGI_REQUEST_COMPLETE = 0;     
   FCGI_CANT_MPX_CONN = 1;     
   FCGI_OVERLOADED = 2;     
   FCGI_UNKNOWN_ROLE = 3;     
   
{
 * Variable names for FCGI_GET_VALUES / FCGI_GET_VALUES_RESULT records
}

   FCGI_MAX_CONNS = 'FCGI_MAX_CONNS';     
   FCGI_MAX_REQS = 'FCGI_MAX_REQS';     
   FCGI_MPXS_CONNS = 'FCGI_MPXS_CONNS';     

type

   FCGI_UnknownTypeBody = record
      _type : byte;
      reserved : array[0..6] of byte;
   end;

   FCGI_UnknownTypeRecord = record
      header : FCGI_Header;
      body : FCGI_UnknownTypeBody;
   end;

implementation

end.
