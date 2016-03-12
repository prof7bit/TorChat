libpurpletorchat.so
===================

  Ponga este fichero .dll en la carpeta del complemento libpurple que es

      %APPDATA%\.purple\plugins\
      (puede que tenga que crear esta carpeta primero)

  o *como alternativa* (no recomendado) puede en su lugar ponerlo junto
a las otras dlls de complementos en la carpeta de programa de Pidgin
(necesitar� permisos de administrador para esto):

      %ProgramFiles%\Pidgin\plugins\

  Adem�s debe tener Tor instalado en la ubicaci�n habitual, esta tiene
que ser una de las siguientes carpetas (ah� es donde el instalador
oficial de Tor instalar� Tor.exe), TorChat buscar� autom�ticamente
estas carpetas y usar� la primera que encuentre:

      %ProgramFiles%\Tor\
      %ProgramFiles%\Vidalia Bundle\Tor\

  S�lo precisa que Tor est� instalado en su computadora, no necesita que
est� ejecut�ndose o que est� configurado de ninguna manera especial para
usar TorChat. TorChat se ocupar� de todo eso autom�ticamente y sin
interferir con otros usos de Tor.

  Ahora inicie pidgin desde una ventana de consola. Debe ver alguna
salida de depuraci�n. Tambi�n puede iniciarlo desde el men� de inicio,
vea debajo c�mo registrar la salida de depuraci�n en un fichero.

  Cree una nueva cuenta, seleccione "TorChat" como protocolo, y cuando
se le pregunte por un nombre de usuario simplemente introduzca su nombre
(o cualquier otra cosa), esto NO es la identificaci�n (ID) en TorChat,
es s�lo un nombre de cuenta para diferenciar perfiles de TorChat con un
nombre legible por humanos. Crear� una carpeta de configuraci�n
%APPDATA%\torchat2_accountname\ para cuenta (perfil). Puede crear tantas
cuentas (perfiles) como quiera y usarlas todas simult�neamente.

Si necesita un fichero de registro para prop�sitos de depuraci�n cree la
carpeta %APPDATA%\torchatlogs antes de iniciarlo, TorChat detectar� esto
y habilitar� el registro para el complemento y para tor, si quiere
deshabilitar el registro simplemente elimine esta carpeta.

El complemento TorChat reconocer� las siguientes variables de entorno
en todas las plataformas: PURPLEHOME, APPDATA, TOR_EXE para evitar las
predeterminadas para el directorio de datos (por si quiere incluirlas
con Pidgin portable), TOR_EXE es la ruta y el nombre del binario tor
(no s�lo la carpeta).
