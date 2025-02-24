-- EFT PROGRMACION EN BASE DE DATOS 1°AÑO
-- ESTUDIANTE: LEONARDO OLIVARES


--*************************************************************************************************************************************
-- Creacion del package PKG_PJE_EXTRA para el cálculo del puntaje extra basado en horas laborales y número de establecimientos.
-- Se define una variable global y una función para calcular el puntaje extra.

CREATE OR REPLACE PACKAGE PKG_PJE_EXTRA AS
    -- Variable global para almacenar el puntaje extra calculado.
    -- Se utilizará para mantener el valor calculado dentro del package.
    V_GLOBAL_PJE_EXTRA NUMBER(10);

    -- Función que calcula el puntaje extra
    -- Retorna:
    --   Un número que representa el puntaje extra calculado.
    FUNCTION FN_CALC_PJE_EX(
        P_SUMA_PJES IN NUMBER,
        P_HRS_TOTALES IN NUMBER,
        P_NUM_ESTABLECIMIENTO IN NUMBER,
        P_PORCEN IN NUMBER
    ) RETURN NUMBER;

END PKG_PJE_EXTRA;
/

--*************************************************************************************************************************************************************************
-- Creacion de package body para la implementación de la función FN_CALC_PJE_EX, que calcula el puntaje extra basado en la cantidad de establecimientos y horas trabajadas.


CREATE OR REPLACE PACKAGE BODY PKG_PJE_EXTRA AS

    -- Implementación de la función FN_CALC_PJE_EX
    FUNCTION FN_CALC_PJE_EX(
        P_SUMA_PJES IN NUMBER,            
        P_HRS_TOTALES IN NUMBER,           
        P_NUM_ESTABLECIMIENTO IN NUMBER,   
        P_PORCEN IN NUMBER                
    ) RETURN NUMBER IS
    BEGIN
        -- Se verifica la condición para asignar el puntaje extra:
        -- Si el postulante trabaja en más de un establecimiento Y supera las 30 horas totales.
        IF (P_NUM_ESTABLECIMIENTO > 1 AND P_HRS_TOTALES > 30) THEN
            -- Se aplica el porcentaje sobre la suma de puntajes base y se redondea el resultado.
            RETURN ROUND(P_SUMA_PJES * (P_PORCEN / 100));
        ELSE
            -- Si no cumple las condiciones, el puntaje extra es 0.
            RETURN 0;
        END IF;
    END FN_CALC_PJE_EX;

END PKG_PJE_EXTRA;
/



--*********************************************************
-- Creación de funcion FN_RUT_FORMAT para formateo de rut 

CREATE OR REPLACE FUNCTION FN_RUT_FORMAT(
    p_numrun IN VARCHAR2,  -- Número del RUN sin formato (sin puntos ni guion)
    p_dvrun  IN VARCHAR2   -- Dígito verificador del RUN
) RETURN VARCHAR2 IS

    -- Relleno el RUN con ceros a la izquierda hasta tener 8 dígitos y se devuelve segun el formato indicado agregando puntos y guion
    v_empty_run     VARCHAR2(20); 
    v_format_run    VARCHAR2(20); 

BEGIN
    -- Rellena con ceros a la izquierda en caso de que el RUN tenga menos de 8 dígitos
    v_empty_run := LPAD(p_numrun, 8, '0');

    -- Formatear el RUN en el estándar chileno XX.XXX.XXX-X
    v_format_run := SUBSTR(v_empty_run, 1, 2) || '.' || 
                    SUBSTR(v_empty_run, 3, 3) || '.' || 
                    SUBSTR(v_empty_run, 6, 3) || '-' || p_dvrun;

    -- Retorna el RUN formateado
    RETURN v_format_run;
END;
/

--***********************************************************************************************************************
-- Creacion de función: FN_PJE_EXP para calcular el puntaje de experiencia de acuerdo a la fecha de contrato más antigua

CREATE OR REPLACE FUNCTION FN_PJE_EXP(
    P_NUMRUN IN VARCHAR2  -- Número del RUN del postulante
) RETURN NUMBER IS

    V_EARLIEST_DATE DATE;    -- Variable para almacenar la fecha de contrato más antigua
    V_ANIOS         NUMBER(2); -- Cantidad de años de experiencia calculados
    V_PJE           NUMBER(10) := 0; -- Variable para almacenar el puntaje de experiencia

BEGIN
    -- Obtener la fecha de contrato más antigua del postulante en ANTECEDENTES_LABORALES
    SELECT MIN(FECHA_CONTRATO)
      INTO V_EARLIEST_DATE
      FROM ANTECEDENTES_LABORALES
     WHERE NUMRUN = P_NUMRUN;
      
    -- Calculo los años de experiencia redondeando hacia abajo
    V_ANIOS := FLOOR(MONTHS_BETWEEN(SYSDATE, V_EARLIEST_DATE) / 12);
        
    -- Obtengo el puntaje correspondiente de acuerdo a los años de experiencia
    SELECT A.PTJE_EXPERIENCIA
      INTO V_PJE
      FROM PTJE_ANNOS_EXPERIENCIA A
     WHERE V_ANIOS BETWEEN A.RANGO_ANNOS_INI AND A.RANGO_ANNOS_TER;
     
    -- Retorna el puntaje calculado
    RETURN V_PJE;
    
EXCEPTION
    -- Capturo la excepción si no encuentra información en las tablas consultadas
    WHEN NO_DATA_FOUND THEN
        -- Registro el error en la tabla ERROR_PROCESO indicando que no hay datos disponibles
        INSERT INTO ERROR_PROCESO (NUMRUN, RUTINA_ERROR, MENSAJE_ERROR)
        VALUES (P_NUMRUN, 'FN_PJE_EXP', 'ORA-01403: No se ha encontrado ningún dato');

        -- Retorna 0 en caso de error
        RETURN 0;

    -- Capturo cualquier otro error inesperado
    WHEN OTHERS THEN
        DECLARE
            v_error_msg VARCHAR2(4000); -- Variable para almacenar el mensaje de error
        BEGIN
            v_error_msg := SQLERRM; -- Obtengo el mensaje de error del sistema
            INSERT INTO ERROR_PROCESO (NUMRUN, RUTINA_ERROR, MENSAJE_ERROR)
            VALUES (P_NUMRUN, 'FN_PJE_EXP', v_error_msg);

            -- Retorna 0 en caso de error
            RETURN 0;
        END;
END;
/

--********************************************************************************************
-- Creacion de función FN_PAIS_PJE para calcular el puntaje asociado al pais del candidato

CREATE OR REPLACE FUNCTION FN_PAIS_PJE(
    P_NUMRUN IN VARCHAR2  -- Número de RUN del postulante
) RETURN NUMBER IS

   V_PJE  NUMBER(10);  -- Variable para almacenar el puntaje del país

BEGIN
   BEGIN
      -- Se calcula el puntaje del país al que postula el candidato
      -- Se realiza un JOIN entre las tablas para identificar la institución y su país correspondiente
      SELECT P.PTJE_PAIS
        INTO V_PJE
        FROM POSTULACION_PASANTIA_PERFEC PP
             INNER JOIN PASANTIA_PERFECCIONAMIENTO PF ON PP.COD_PROGRAMA = PF.COD_PROGRAMA
             INNER JOIN INSTITUCION I ON PF.COD_INST = I.COD_INST
             INNER JOIN PTJE_PAIS_POSTULA P ON I.COD_PAIS = P.COD_PAIS
       WHERE PP.NUMRUN = P_NUMRUN;

   EXCEPTION
      -- Manejo de errores si no se encuentran datos para el RUN ingresado
      WHEN NO_DATA_FOUND THEN
         -- Se registra el error en la tabla ERROR_PROCESO y se asigna puntaje 0
         INSERT INTO ERROR_PROCESO(NUMRUN, RUTINA_ERROR, MENSAJE_ERROR)
         VALUES (P_NUMRUN, 'FN_PAIS_PJE', 'Puntaje no definido para el NUMRUN=' || P_NUMRUN);
         V_PJE := 0;

      -- Manejo de cualquier otro error inesperado
      WHEN OTHERS THEN
         DECLARE
            V_ERROR_MSG VARCHAR2(2000);  -- Variable para almacenar el mensaje de error
         BEGIN
            V_ERROR_MSG := SQLERRM;  -- Captura el mensaje de error del sistema
            INSERT INTO ERROR_PROCESO(NUMRUN, RUTINA_ERROR, MENSAJE_ERROR)
            VALUES (P_NUMRUN, 'FN_PAIS_PJE', V_ERROR_MSG);
            V_PJE := 0;  -- Asigno 0 en caso de error
         END;
   END;

   -- Retorna el puntaje obtenido o 0 en caso de error
   RETURN V_PJE;
END;
/



--*********************************************************************************************************************************************************************************
-- Creacion de procedimiento almacenado PROC_CALC_PJES para iterar sobre los postulantes, calcula los puntajes de cada segmento y  guarda la información en las tablas respectivas.

CREATE OR REPLACE PROCEDURE PROC_CALC_PJES (
    P_PORCEN IN NUMBER  -- Parámetro que define el porcentaje de incremento para el puntaje extra
) IS

    -- Cursor que obtiene la lista de postulantes ordenados por RUN
    CURSOR C_POSTULANTES IS
        SELECT NUMRUN,
               DVRUN,
               -- Construcción del nombre completo con primera letra en mayúscula
               INITCAP(PNOMBRE) || ' ' || INITCAP(NVL(SNOMBRE, '')) || ' ' ||
               INITCAP(APATERNO) || ' ' || INITCAP(AMATERNO) AS NOMBRE_COMPLETO
          FROM ANTECEDENTES_PERSONALES
         ORDER BY NUMRUN;
         
    -- Variables para almacenar los valores de cada postulante
    V_NUMRUN       VARCHAR2(20);  -- RUN formateado
    V_NOMBRE       VARCHAR2(200); -- Nombre completo del postulante
    V_PTJE_EXP     NUMBER(10);    -- Puntaje por años de experiencia
    V_PTJE_PAIS    NUMBER(10);    -- Puntaje por país al que postula
    V_SUM_HRS      NUMBER(10);    -- Total de horas semanales trabajadas
    V_CANT_ESTABLEC NUMBER(10);   -- Cantidad de establecimientos donde trabaja el postulante
    V_PJE_EXTRA    NUMBER(10);    -- Puntaje extra calculado

BEGIN
    -- Limpio las tablas de salida antes de procesar los datos
    EXECUTE IMMEDIATE 'TRUNCATE TABLE DETALLE_PUNTAJE_POSTULACION';
    EXECUTE IMMEDIATE 'TRUNCATE TABLE RESULTADO_POSTULACION';
    EXECUTE IMMEDIATE 'TRUNCATE TABLE ERROR_PROCESO';

    -- Recorrer la lista de postulantes
    FOR rec IN C_POSTULANTES LOOP
        -- Formateo el RUN utilizando la funcion FN_RUT_FORMA creada anteriormente
        V_NUMRUN := FN_RUT_FORMAT(rec.NUMRUN, rec.DVRUN);
        V_NOMBRE := rec.NOMBRE_COMPLETO;
      
        -- Caluclo puntajes de experiencia y país usando las funciones creadasa anteriormente
        V_PTJE_EXP  := FN_PJE_EXP(rec.NUMRUN);
        V_PTJE_PAIS := FN_PAIS_PJE(rec.NUMRUN);
    
        BEGIN
            -- Obtengo suma de horas trabajadas y cantidad de establecimientos
            SELECT NVL(SUM(HORAS_SEMANALES), 0), NVL(COUNT(*), 0)
              INTO V_SUM_HRS, V_CANT_ESTABLEC
              FROM ANTECEDENTES_LABORALES
             WHERE NUMRUN = rec.NUMRUN;

        EXCEPTION
            -- Si no se encuentran datos, se asignan valores predeterminados
            WHEN NO_DATA_FOUND THEN
                V_SUM_HRS  := 0;
                V_CANT_ESTABLEC := 0;
        END;
        
        -- Se calcula el puntaje extra usando la función del package, sumando los puntajes de experiencia y país
        V_PJE_EXTRA  := PKG_PJE_EXTRA.FN_CALC_PJE_EX(
                            P_SUMA_PJES => V_PTJE_EXP + V_PTJE_PAIS,
                            P_HRS_TOTALES => V_SUM_HRS,
                            P_NUM_ESTABLECIMIENTO => V_CANT_ESTABLEC,
                            P_PORCEN => P_PORCEN
                        );

        -- Se almacena el puntaje extra calculado en la variable pública del package
        PKG_PJE_EXTRA.V_GLOBAL_PJE_EXTRA := V_PJE_EXTRA;
    
        -- Inserción de datos en tabla DETALLE_PUNTAJE_POSTULACION
        INSERT INTO DETALLE_PUNTAJE_POSTULACION (
            RUN_POSTULANTE,
            NOMBRE_POSTULANTE,
            PTJE_ANNOS_EXP,
            PTJE_PAIS_POSTULA,
            PTJE_EXTRA
        ) VALUES (
            V_NUMRUN,
            V_NOMBRE,
            V_PTJE_EXP,
            V_PTJE_PAIS,
            V_PJE_EXTRA
        );
    END LOOP;
END;
/

--***************************************************************************************************************************************
-- Creacion de trigger: TRIG_POST_DETALL_PJE para calcular el puntaje final y actualizar o insertar en la tabla RESULTADO_POSTULACION.

CREATE OR REPLACE TRIGGER TRIG_POST_DETALL_PJE
AFTER INSERT ON DETALLE_PUNTAJE_POSTULACION  -- Se ejecuta después de cada inserción en DETALLE_PUNTAJE_POSTULACION
FOR EACH ROW  -- Se ejecuta para cada fila insertada
DECLARE
    V_PJE_FINAL NUMBER(10);   -- Variable para almacenar el puntaje final
    V_RESULT    VARCHAR2(20); -- Variable para almacenar el resultado de la postulación
BEGIN
    -- Calcula el puntaje final sumando los puntajes de experiencia, país y extra
    V_PJE_FINAL := :NEW.PTJE_ANNOS_EXP + :NEW.PTJE_PAIS_POSTULA + :NEW.PTJE_EXTRA;
    
    -- Verifica si el postulante es seleccionado según el puntaje
    IF V_PJE_FINAL >= 2500 THEN
        V_RESULT := 'SELECCIONADO';
    ELSE
        V_RESULT := 'NO SELECCIONADO';
    END IF;
    
    -- Verifica si la postulacion ya existe para actualizar el resultado
    UPDATE RESULTADO_POSTULACION
       SET PTJE_FINAL_POST = V_PJE_FINAL,
           RESULTADO_POST  = V_RESULT
     WHERE RUN_POSTULANTE = :NEW.RUN_POSTULANTE;
     
    -- Si no se actualizó ningún registro (es decir, el RUN_POSTULANTE no existía en la tabla),
    -- se inserta un nuevo registro con el resultado de la postulación.
    IF SQL%ROWCOUNT = 0 THEN
        INSERT INTO RESULTADO_POSTULACION (
            RUN_POSTULANTE,
            PTJE_FINAL_POST,
            RESULTADO_POST
        ) VALUES (
            :NEW.RUN_POSTULANTE,
            V_PJE_FINAL,
            V_RESULT
        );
    END IF;
END;
/


--****************************************************************
-- Llamada al procedimiennto almacenado con un porcentaje de 35%
BEGIN
    PROC_CALC_PJES(35);  
END;
/

--*********************************************************************************************
--Verificacion de resultados en tablas

--**********************************************************************************************
--Tabla : DETALLE_PUNTAJE_POSTULACION
SELECT 
    run_postulante AS "RUN_POSTULANTE",
    nombre_postulante AS "NOMBRE_POSTULANTE",
    ptje_annos_exp AS "PTJE_ANNOS_EXP",
    ptje_pais_postula AS "PTJE_PAIS_POSTULA",
    ptje_extra AS "PTJE_EXTRA"
FROM 
    DETALLE_PUNTAJE_POSTULACION
ORDER BY 
    TO_NUMBER(REPLACE(SUBSTR(RUN_POSTULANTE, 1, INSTR(RUN_POSTULANTE, '-') - 1), '.', ''));

--*********************************************************************************************
--Tabla RESULTADO_POSTULACION
SELECT * FROM RESULTADO_POSTULACION ORDER BY run_postulante;

--*********************************************************************************************

--Tabla ERROR_PROCESO
SELECT * FROM ERROR_PROCESO;
