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
-- //// TASA DE DESOCUPACION (PD)

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

-- //////////////////////////////////////////////////////////////////// 
-- UNION DE TABLAS

SELECT * FROM cetes
SELECT * FROM inflacion
SELECT * FROM pib
SELECT * FROM tc
SELECT * FROM td


-- Guardamos en una VIEW las tablas que hemos creado, usando como "columna join" la fecha
CREATE VIEW view_database AS
SELECT 
A.fecha,
A.tasa_cetes,
B.inpc,
C.pib,
D.tc,
E.td
FROM cetes AS A
LEFT JOIN inflacion AS B
ON A.fecha=B.fecha
LEFT JOIN pib AS C
ON A.fecha=C.fecha
LEFT JOIN tc AS D
ON A.fecha=D.fecha
LEFT JOIN td AS E
ON A.fecha=E.fecha

-- Visualizamos la view creada
SELECT * FROM view_database;

-- Corregimos un error que hemos venido arrastrando desde antes, las variables macroeconómicas ahora
-- las cambiamos a variable tipo "numeric", en lugar de tipo "char"

CREATE TABLE data_base AS
SELECT 
fecha,
cast(tasa_cetes as numeric)/100 AS tasa_cetes, --4 DECIMALES
cast(inpc as numeric)/100 AS inflacion, --4 DECIMALES
cast(pib as numeric)/100 AS pib, --5 DECIMALES
ln(cast(tc as numeric)) AS ln_tc, -- 5 DECIMALES
cast(td as numeric)/100 AS td --5 DECIMALES
FROM view_database;

-- Visualizamos la tabla final
SELECT * FROM data_base;


SELECT 
fecha,
ROUND(cast(tasa_cetes as numeric)/100,4) AS tasa_cetes,
ROUND(cast(inpc as numeric)/100,4) AS inflacion,
ROUND(cast(pib as numeric)/100,5) AS pib,
ROUND(ln(cast(tc as numeric)),5) AS ln_tc,
ROUND(cast(td as numeric)/100,5) AS td
FROM view_database;
