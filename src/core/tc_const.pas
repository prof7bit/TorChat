unit tc_const;

{$mode objfpc}{$H+}

interface

const
  SOFTWARE_NAME : String = 'TorChat';
  SOFTWARE_VERSION : String = '2.0-alpha-11';

  SECONDS_SEND_KEEPLIVE = 120;   // don't change, protocol specification
  SECONDS_WAIT_KEEPALIVE = 240;  // don't change, protocol specification

  SECONDS_WAIT_FOR_PONG = 240;   // incoming conn timeout, should be 240
  SECONDS_KEEP_ON_TEMPLIST = 240;
  SECONDS_WAIT_FOR_HOSTNAME_FILE = 20;
  SECONDS_INITIAL_RECONNECT = 10;
  SECONDS_FIRST_CONNECT = 5;     // first connect after TBuddy.Create

  FILE_TRANSFER_BLOCK_SIZE = 8192;
  FILE_TRANSFER_BLOCKS_WAIT = 16;


implementation

end.

