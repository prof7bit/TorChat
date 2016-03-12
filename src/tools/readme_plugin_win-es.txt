libpurpletorchat.so
===================

  Ponga este fichero .dll en la carpeta del complemento libpurple que es

      %APPDATA%\.purple\plugins\
      (puede que tenga que crear esta carpeta primero)

  o *como alternativa* (no recomendado) puede en su lugar ponerlo junto
a las otras dlls de complementos en la carpeta de programa de Pidgin
(necesitará permisos de administrador para esto):

      %ProgramFiles%\Pidgin\plugins\

  Además debe tener Tor instalado en la ubicación habitual, esta tiene
que ser una de las siguientes carpetas (ahí es donde el instalador
oficial de Tor instalará Tor.exe), TorChat buscará automáticamente
estas carpetas y usará la primera que encuentre:

      %ProgramFiles%\Tor\
      %ProgramFiles%\Vidalia Bundle\Tor\

  Sólo precisa que Tor esté instalado en su computadora, no necesita que
esté ejecutándose o que esté configurado de ninguna manera especial para
usar TorChat. TorChat se ocupará de todo eso automáticamente y sin
interferir con otros usos de Tor.

  Ahora inicie pidgin desde una ventana de consola. Debe ver alguna
salida de depuración. También puede iniciarlo desde el menú de inicio,
vea debajo cómo registrar la salida de depuración en un fichero.

  Cree una nueva cuenta, seleccione "TorChat" como protocolo, y cuando
se le pregunte por un nombre de usuario simplemente introduzca su nombre
(o cualquier otra cosa), esto NO es la identificación (ID) en TorChat,
es sólo un nombre de cuenta para diferenciar perfiles de TorChat con un
nombre legible por humanos. Creará una carpeta de configuración
%APPDATA%\torchat2_accountname\ para cuenta (perfil). Puede crear tantas
cuentas (perfiles) como quiera y usarlas todas simultáneamente.

Si necesita un fichero de registro para propósitos de depuración cree la
carpeta %APPDATA%\torchatlogs antes de iniciarlo, TorChat detectará esto
y habilitará el registro para el complemento y para tor, si quiere
deshabilitar el registro simplemente elimine esta carpeta.

El complemento TorChat reconocerá las siguientes variables de entorno
en todas las plataformas: PURPLEHOME, APPDATA, TOR_EXE para evitar las
predeterminadas para el directorio de datos (por si quiere incluirlas
con Pidgin portable), TOR_EXE es la ruta y el nombre del binario tor
(no sólo la carpeta).
