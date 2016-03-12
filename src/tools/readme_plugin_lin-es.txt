libpurpletorchat.so
===================

  Ponga este fichero en la carpeta de complementos de libpurple que es

      ~/.purple/plugins/
      (puede que primero tenga que crear esta carpeta)

  o como alternativa póngalo en

      /usr/lib/purple-2/

  Además debe tener tor instalado. TorChat buscará estas rutas y usará
la que encuentre primero:

      /usr/local/sbin/
      /usr/local/bin/
      /usr/sbin/
      /usr/bin/
      /sbin/
      /bin/

  No precisa que Tor necesariamente esté ejecutándose o esté configurado
de ninguna forma especial para usar TorChat, TorChat se ocupará de todo
eso automáticamente. Iniciará un proceso aparte con su propia
configuración que no interferirá ni puede interferir con su configuración
de tor existente y su uso.  

  Ahora inicie pidgin desde el interior de una ventana de consola. Debe
ver alguna salida de depuración si lo inició desde la consola.

  Cree una cuenta nueva, seleccione "TorChat" como protocolo, y cuando
se le pregunte por un nombre de usuario simplemente introduzca su nombre
(o cualquier otra cosa), esto NO es la identificación (ID) en TorChat,
sólo es un nombre de cuenta para diferenciar perfiles de TorChat mediante
un nombre legible por humanos. Esto creará una carpeta de configuración
~/.torchat2_accountname/ para cada cuenta (perfil). Puede crear tantas
cuentas (perfiles) como quiera y usarlas todas simultáneamente.

Si necesita un fichero de registro (log) para propósitos de depuración
cree la carpeta ~/torchatlogs antes de iniciarlo, TorChat detectará esto
y habilitará el registro para el complemento y para tor, si quiere
deshabilitar el registro simplemente elimine esta carpeta.

El complemento TorChat reconocerá las siguientes variables de entorno
en todas las plataformas: PURPLEHOME, APPDATA, TOR_EXE para evitar las
predeterminadas para el directorio de datos (por si quiere incluirlas
con Pidgin portable). TOR_EXE es la ruta y el nombre del binario tor
(no sólo la carpeta).
