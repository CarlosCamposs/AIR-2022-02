-- ////////////////////////////////////////////////////////////////////
-- Proyecto 2 
-- Administracion Integral de Riesgos


-- //////////////////////////////////////////////////////////////////// 
-- //// INFLACION

-- Otra forma de tratar con las fechas, aqui no la tratamos como DATE al definir la tabla
-- y solo hacemos substrings para determinar el año

-- Creamos el esqueleto de la tabla
CREATE TABLE tabla_inflacion(
	Fecha varchar PRIMARY KEY,
	INPC varchar);

-- Importamos los datos de inflación en la tabla
SELECT * FROM tabla_inflacion;

-- Creamos una view segregando año y mes de cada registro
CREATE VIEW view_inflacion
AS
SELECT *, 
	SUBSTRING(fecha FROM 4 FOR 2)AS month,
	SUBSTRING(fecha FROM 7 FOR 4)AS year
FROM tabla_inflacion;

-- Visualizamos la view creada
SELECT * FROM view_inflacion;

-- Eliminamos los registros que no son de nuestro interes
DELETE FROM view_inflacion WHERE  year<'2000' OR year>'2021';
DELETE FROM view_inflacion WHERE month<'12' and year='2000'

-- Finalmente creamos la tabla con los datos de inflacion
CREATE TABLE inflacion AS
SELECT fecha, inpc FROM view_inflacion;

SELECT * FROM inflacion;

-- //////////////////////////////////////////////////////////////////// 
-- //// TASAS CETES

-- Creamos el esqueleto de la tabla
CREATE TABLE tabla_cetes(
	Fecha varchar PRIMARY KEY,
	tasa_cetes varchar);

-- Importamos los datos de tasa cetes en la tabla
SELECT * FROM tabla_cetes;

-- Creamos una view segregando año y mes de cada registro
CREATE VIEW view_cetes
AS
SELECT *, 
	SUBSTRING(fecha FROM 4 FOR 2)AS month,
	SUBSTRING(fecha FROM 7 FOR 4)AS year
FROM tabla_cetes;

-- Visualizamos la view creada
SELECT * FROM view_cetes;

-- Eliminamos los registros que no son de nuestro interes
DELETE FROM view_cetes WHERE  year<'2000' OR year>'2021';
DELETE FROM view_cetes WHERE month<'12' and year='2000'

-- Finalmente creamos la tabla con los datos de inflacion
CREATE TABLE cetes AS
SELECT fecha, tasa_cetes FROM view_cetes;

SELECT * FROM cetes;


-- //////////////////////////////////////////////////////////////////// 
-- //// TIPO DE CAMBIO (FIX, al final)

-- Creamos el esqueleto de la tabla
CREATE TABLE tabla_tc(
	Fecha varchar PRIMARY KEY,
	tc varchar);

-- Importamos los datos del tipo de cambio fix en la tabla
SELECT * FROM tabla_tc;

-- Creamos una view segregando año y mes de cada registro
CREATE VIEW view_tc
AS
SELECT *, 
	SUBSTRING(fecha FROM 4 FOR 2)AS month,
	SUBSTRING(fecha FROM 7 FOR 4)AS year
FROM tabla_tc;

-- Visualizamos la view creada
SELECT * FROM view_tc;

-- Eliminamos los registros que no son de nuestro interes
DELETE FROM view_tc WHERE  year<'2000' OR year>'2021';
DELETE FROM view_tc WHERE month<'12' and year='2000'

-- Finalmente creamos la tabla con los datos de inflacion
CREATE TABLE tc AS
SELECT fecha, tc FROM view_tc;

SELECT * FROM tc;



-- //////////////////////////////////////////////////////////////////// 
-- //// PRODUCTO INTERNO BRUTO

-- Creamos el esqueleto de la tabla
CREATE TABLE tabla_pib(
	Fecha varchar PRIMARY KEY,
	pib varchar); -- Ya cree esta tabla ayer en la noche

SELECT * FROM tabla_pib;

-- Creamos una view segregando año y mes de cada registro
CREATE VIEW view_pib
AS
SELECT *, 
	SUBSTRING(fecha FROM 4 FOR 2)AS month,
	SUBSTRING(fecha FROM 7 FOR 4)AS year
FROM tabla_pib;

-- Visualizamos la view creada
SELECT * FROM view_pib;

-- Eliminamos los registros que no son de nuestro interes
DELETE FROM view_pib WHERE  year<'2000' OR year>'2021';
DELETE FROM view_pib WHERE month<'12' and year='2000'

-- Finalmente creamos la tabla con los datos de inflacion
CREATE TABLE pib AS
SELECT fecha, pib FROM view_pib;

SELECT * FROM pib;


-- //////////////////////////////////////////////////////////////////// 
-- //// TASA DE DESOCUPACION (TD)

-- Creamos el esqueleto de la tabla
CREATE TABLE tabla_td(
	Fecha varchar PRIMARY KEY,
	td varchar); 

SELECT * FROM tabla_td;

-- Creamos una view segregando año y mes de cada registro
CREATE VIEW view_td
AS
SELECT *, 
	SUBSTRING(fecha FROM 4 FOR 2)AS month,
	SUBSTRING(fecha FROM 7 FOR 4)AS year
FROM tabla_td;

-- Visualizamos la view creada
SELECT * FROM view_td;

-- Eliminamos los registros que no son de nuestro interes
DELETE FROM view_td WHERE  year>'2021';

-- Finalmente creamos la tabla con los datos de inflacion
CREATE TABLE td AS
SELECT fecha, td FROM view_td;

SELECT * FROM td;

--//////////////////////////////////////////////////////////////////// 
-- //// INDICE DE PRECIOS Y COTIZACIONES

-- Creamos el esqueleto de la tabla
CREATE TABLE tabla_ipc(
	fecha varchar PRIMARY KEY,
	ipc varchar);

-- Importamos los datos del ipc en la tabla
SELECT * FROM tabla_ipc;

-- Creamos una view segregando año y mes de cada registro
CREATE VIEW view_ipc
AS
SELECT *, 
	SUBSTRING(fecha FROM 4 FOR 2)AS month,
	SUBSTRING(fecha FROM 7 FOR 4)AS year
FROM tabla_ipc;

-- Visualizamos la view creada
SELECT * FROM view_ipc;

-- Eliminamos los registros que no son de nuestro interes
DELETE FROM view_ipc WHERE  year<'2000' OR year>'2021';
DELETE FROM view_ipc WHERE month<'12' and year='2000';

-- Finalmente creamos la tabla con los datos de inflacion
CREATE TABLE ipc AS
SELECT fecha, ipc FROM view_ipc;

SELECT * FROM ipc;

-- //////////////////////////////////////////////////////////////////// 
-- //// Actividad Industrial

-- Creamos el esqueleto de la tabla
CREATE TABLE tabla_actvind(
	Fecha varchar PRIMARY KEY,
	actv_industrial varchar);

-- Importamos los datos de la actividad industrial en la tabla
SELECT * FROM tabla_actvind;

-- Creamos una view segregando año y mes de cada registro
CREATE VIEW view_actvind
AS
SELECT *, 
	SUBSTRING(fecha FROM 4 FOR 2)AS month,
	SUBSTRING(fecha FROM 7 FOR 4)AS year
FROM tabla_actvind;

-- Visualizamos la view creada
SELECT * FROM view_actvind;

-- Eliminamos los registros que no son de nuestro interes
DELETE FROM view_actvind WHERE  year<'2000' OR year>'2021';
DELETE FROM view_actvind WHERE month<'12' and year='2000'

-- Finalmente creamos la tabla con los datos de la actividad industrial
CREATE TABLE actvind AS
SELECT fecha, actv_industrial FROM view_actvind;

SELECT * FROM actvind;

-- //////////////////////////////////////////////////////////////////// 
-- //// INPP

-- Creamos el esqueleto de la tabla
CREATE TABLE tabla_inpp(
	Fecha varchar PRIMARY KEY,
	inpp varchar);

-- Importamos los datos del inpp en la tabla
SELECT * FROM tabla_inpp;

-- Creamos una view segregando año y mes de cada registro
CREATE VIEW view_inpp
AS
SELECT *, 
	SUBSTRING(fecha FROM 4 FOR 2)AS month,
	SUBSTRING(fecha FROM 7 FOR 4)AS year
FROM tabla_inpp;

-- Visualizamos la view creada
SELECT * FROM view_inpp;

-- Eliminamos los registros que no son de nuestro interes
DELETE FROM view_inpp WHERE  year<'2000' OR year>'2021';
DELETE FROM view_inpp WHERE month<'12' and year='2000'

-- Finalmente creamos la tabla con los datos de inflacion
CREATE TABLE inpp AS
SELECT fecha, inpp FROM view_inpp;

SELECT * FROM inpp;

-- //////////////////////////////////////////////////////////////////// 
-- //// Ahorro total 

-- Creamos el esqueleto de la tabla
CREATE TABLE tabla_ahorro(
	fecha varchar PRIMARY KEY,
	ahorro varchar);

-- Importamos los datos de ahorro total en la tabla
SELECT * FROM tabla_ahorro;

-- Creamos una view segregando año y mes de cada registro
CREATE VIEW view_ahorro
AS
SELECT *, 
	SUBSTRING(fecha FROM 4 FOR 2)AS month,
	SUBSTRING(fecha FROM 7 FOR 4)AS year
FROM tabla_ahorro;

-- Visualizamos la view creada
SELECT * FROM view_ahorro;

-- Eliminamos los registros que no son de nuestro interes
DELETE FROM view_ahorro WHERE  year<'2000' OR year>'2021';
DELETE FROM view_ahorro WHERE month<'12' and year='2000'

-- Finalmente creamos la tabla con los datos de ahorro
CREATE TABLE ahorro AS
SELECT fecha, ahorro FROM view_ahorro;

SELECT * FROM ahorro;

-- //////////////////////////////////////////////////////////////////// 
-- //// EXPORTACIONES

-- Creamos el esqueleto de la tabla
CREATE TABLE tabla_exportacion(
	fecha varchar PRIMARY KEY,
	exportaciones varchar);

-- Importamos los datos de tasa cetes en la tabla
SELECT * FROM tabla_exportacion;

-- Creamos una view segregando año y mes de cada registro
CREATE VIEW view_exportacion
AS
SELECT *, 
	SUBSTRING(fecha FROM 4 FOR 2)AS month,
	SUBSTRING(fecha FROM 7 FOR 4)AS year
FROM tabla_exportacion;

-- Visualizamos la view creada
SELECT * FROM view_exportacion;

-- Eliminamos los registros que no son de nuestro interes
DELETE FROM view_exportacion WHERE  year<'2000' OR year>'2021';
DELETE FROM view_exportacion WHERE month<'12' and year='2000'

-- Finalmente creamos la tabla con los datos de inflacion
CREATE TABLE exportaciones AS
SELECT fecha, exportaciones FROM view_exportacion;

SELECT * FROM exportaciones;

-- //////////////////////////////////////////////////////////////////// 
-- //// IMPORTACIONES

-- Creamos el esqueleto de la tabla
CREATE TABLE tabla_importacion(
	fecha varchar PRIMARY KEY,
	importaciones varchar);

-- Importamos los datos de importaciones en la tabla
SELECT * FROM tabla_importacion;

-- Creamos una view segregando año y mes de cada registro
CREATE VIEW view_importacion
AS
SELECT *, 
	SUBSTRING(fecha FROM 4 FOR 2)AS month,
	SUBSTRING(fecha FROM 7 FOR 4)AS year
FROM tabla_importacion;

-- Visualizamos la view creada
SELECT * FROM view_importacion;

-- Eliminamos los registros que no son de nuestro interes
DELETE FROM view_importacion WHERE  year<'2000' OR year>'2021';
DELETE FROM view_importacion WHERE month<'12' and year='2000';

-- Finalmente creamos la tabla con los datos de importaciones
CREATE TABLE importaciones AS
SELECT fecha, importaciones FROM view_importacion;

SELECT * FROM importaciones;


-- //////////////////////////////////////////////////////////////////// 
-- //// INVERSIÓN FIJA

-- Creamos el esqueleto de la tabla
CREATE TABLE tabla_inversion(
	fecha varchar PRIMARY KEY,
	inversion varchar);

-- Importamos los datos de inversion fija en la tabla
SELECT * FROM tabla_inversion;

-- Creamos una view segregando año y mes de cada registro
CREATE VIEW view_inversion
AS
SELECT *, 
	SUBSTRING(fecha FROM 4 FOR 2)AS month,
	SUBSTRING(fecha FROM 7 FOR 4)AS year
FROM tabla_inversion;

-- Visualizamos la view creada
SELECT * FROM view_inversion;

-- Eliminamos los registros que no son de nuestro interes
DELETE FROM view_inversion WHERE  year<'2000' OR year>'2021';
DELETE FROM view_inversion WHERE month<'12' and year='2000';

-- Finalmente creamos la tabla con los datos de importaciones
CREATE TABLE inversion AS
SELECT fecha, inversion FROM view_inversion;

SELECT * FROM inversion;



-- //////////////////////////////////////////////////////////////////// 
-- UNION DE TABLAS

SELECT * FROM cetes;
SELECT * FROM inflacion;
SELECT * FROM pib;
SELECT * FROM tc;
SELECT * FROM td;
SELECT * FROM ipc;
SELECT * FROM actvind;
SELECT* FROM inpp;
SELECT * FROM ahorro;
SELECT * FROM exportaciones;
SELECT * FROM importaciones;
SELECT * FROM inversion;


-- Guardamos en una VIEW las tablas que hemos creado, usando como "columna join" la fecha
CREATE VIEW view_database AS
SELECT 
A.fecha,
A.tasa_cetes,
B.inpc,
C.pib,
D.tc,
E.td,
F.ipc,
G.actv_industrial,
H.inpp,
I.ahorro,
J.exportaciones,
K.importaciones,
L.inversion
FROM cetes AS A
LEFT JOIN inflacion AS B
ON A.fecha=B.fecha
LEFT JOIN pib AS C
ON A.fecha=C.fecha
LEFT JOIN tc AS D
ON A.fecha=D.fecha
LEFT JOIN td AS E
ON A.fecha=E.fecha
LEFT JOIN ipc AS F
ON A.fecha=F.fecha
LEFT JOIN actvind AS G
ON A.fecha=G.fecha
LEFT JOIN inpp AS H
ON A.fecha=H.fecha
LEFT JOIN ahorro AS I
ON A.fecha=I.fecha
LEFT JOIN exportaciones AS J
ON A.fecha=J.fecha
LEFT JOIN importaciones AS K
ON A.fecha=K.fecha
LEFT JOIN inversion AS L
ON A.fecha=L.fecha;

-- Visualizamos la view creada
SELECT * FROM view_database;

-- Corregimos un error que hemos venido arrastrando desde antes, las variables macroeconómicas ahora
-- las cambiamos a variable tipo "numeric", en lugar de tipo "char"

CREATE TABLE database AS
SELECT 
SUBSTRING(fecha FROM 4 FOR 7) AS fecha,
ROUND(cast(tasa_cetes as numeric)/100,4) AS tasa_cetes,
ROUND(cast(inpc as numeric)/100,4) AS inflacion,
ROUND(cast(pib as numeric)/100,5) AS pib,
ROUND(ln(cast(tc as numeric)),5) AS ln_tc,
ROUND(cast(td as numeric)/100,5) AS td,
ROUND(ln(cast(ipc as numeric)),5) AS ln_ipc,
ROUND(ln(cast(actv_industrial as numeric)),5) AS ln_actvindustrial,
ROUND(ln(cast(inpp as numeric)),5) AS ln_inpp,
ROUND(cast(ahorro as numeric)/100,4) AS ahorro,
ROUND(ln(cast(exportaciones as numeric)),5) AS ln_export,
ROUND(ln(cast(importaciones as numeric)),5) AS ln_import,
ROUND(cast(inversion as numeric)/100,4) AS inversion
FROM view_database;


-- Visualizamos la tabla final
SELECT * FROM database;




