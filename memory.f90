! Juego memory
! Reglas del juego:
!	1.-	Hay un tablero de FILAS*COLUMNAS (matriz cuadrada con un número par de elementos)
!	2.-	Estarán los elementos del 1 al (FILAS*COLUMNAS)/2 colocados en parejas de forma aleatoria en el tablero
!	3.-	Hay que buscar las parejas en el tablero
!	4.-	Se ganará cuando se hayan encontrado todos los elementos
! Autor: Francis

program memory
	INTEGER, PARAMETER :: FILAS = 4, COLUMNAS = 4
	INTEGER, dimension(FILAS,COLUMNAS) :: tablerojuego, tableromostrar
	
	! Genero el tablero del juego
	! Genero una matriz de FILAS*COLUMNAS donde habrá (FILAS*COLUMNAS)/2 elementos, es decir, habrá un elemento en dos casillas, que es el que hay que buscar
	CALL generarTableroJuego(tablerojuego,FILAS,COLUMNAS,tableromostrar)
	
	! Jugamos
	CALL jugar(tablerojuego,FILAS,COLUMNAS,tableromostrar)
	
	! Funciones y procedimientos del juego
	CONTAINS

	!*******************************************
	!** Juego                                 **
	!** Parámetros:                           **
	!** 	matriz: matriz                    **
	!** 	filas: filas de la matriz         **
	!** 	columnas: columnas de la matriz   **
	!**		tableromostrar: tablero a mostrar **
	!*******************************************

	SUBROUTINE jugar(matriz, filas, columnas, tableromostrar)
		INTEGER, DIMENSION(:,:), INTENT(OUT) :: matriz, tableromostrar
		INTEGER, INTENT(IN) :: filas, columnas
		integer :: aciertos, aciertosparaganar, posicionx1, posiciony1, posicionx2, posiciony2
		
		aciertos = 0
		aciertosparaganar = FILAS*COLUMNAS
	
		DO WHILE ( aciertos < aciertosparaganar )
			PRINT*," ********** Tablero **********"
			CALL mostrarMatriz(tableromostrar,FILAS,COLUMNAS)
			PRINT*," PUNTUACION: ", aciertos
			
			! Leo la posición de los elementos a mirar
			PRINT*, "Introduce la posicion x del elemento a mirar primero"
			READ*, posicionx1
			PRINT*, "Introduce la posicion y del elemento a mirar primero"
			READ*, posiciony1
			PRINT*, "Introduce la posicion x del elemento a mirar segundo"
			READ*, posicionx2
			PRINT*, "Introduce la posicion y del elemento a mirar segundo"
			READ*, posiciony2
			
			! Muestro los elementos seleccionados
			PRINT*," ********************************************* "
			PRINT*," tablero[",posicionx1,"][",posiciony1,"] = ", tablerojuego(posicionx1,posiciony1)
			PRINT*," tablero[",posicionx2,"][",posiciony2,"] = ", tablerojuego(posicionx2,posiciony2)
			PRINT*," ********************************************* "
			
			! Miro si los elementos son iguales
			IF ( tablerojuego(posicionx1,posiciony1) == tablerojuego(posicionx2,posiciony2) ) THEN
				PRINT*,"Acertaste!!!!"
				aciertos = aciertos + 2
				tableromostrar(posicionx1,posiciony1) = tablerojuego(posicionx1,posiciony1);
				tableromostrar(posicionx2,posiciony2) = tablerojuego(posicionx2,posiciony2);
			ELSE
				PRINT*,"Vaya, has fallado!!!!"
			ENDIF
		ENDDO
	
	END SUBROUTINE jugar

	!*******************************************
	!** Genera el tablero del juego           **
	!** Parámetros:                           **
	!** 	matriz: matriz                    **
	!** 	filas: filas de la matriz         **
	!** 	columnas: columnas de la matriz   **
	!**		tableromostrar: tablero a mostrar **
	!*******************************************

	SUBROUTINE generarTableroJuego(matriz, filas, columnas, tableromostrar)
		INTEGER, DIMENSION(:,:), INTENT(OUT) :: matriz, tableromostrar
		INTEGER, INTENT(IN) :: filas, columnas
		INTEGER :: i, j, tope, filaaleatoria, columnaaleatoria
		LOGICAL :: parar
		
		! tope nos dice cuántos elementos diferentes habrá
		! si la matriz es de 4*4 hay 16 elementos, como van a ir en parejas de 2 para ir buscándolos hay que pones 16/2 = 8 elementos
		tope = (filas*columnas)/2

		! Primero lleno la matriz de -1, con el -1 vamos a entender que no hay ningún elemento en esa posición, es decir, está vacía
		DO i = 1,filas
			DO j = 1,columnas
				matriz(i,j) = -1
				tableromostrar(i,j) = 0
			ENDDO
		ENDDO
		
		! Aquí voy a poner los números en posiciones aleatorias, habrá 2 posiciones en la matriz para cada elemento
		DO i = 1, tope
			!print*,"Voy a generar las dos posiciones aleatorias dentro de la matriz para el ",i
			parar = .false.
			! Genero la primera posición del número
			DO WHILE (parar .eqv. .false. )
				filaaleatoria = aleatorioEntre(1, filas+1)				! Genero una posición aleatoria para la fila
				columnaaleatoria = aleatorioEntre(1, columnas+1)		! Genero una posición aleatoria para la columna
				IF (matriz(filaaleatoria,columnaaleatoria) == -1) THEN	! Si la posición está vacía (vale -1)
					matriz(filaaleatoria,columnaaleatoria) = i			! Coloco ahí ese número
					!print*, "He puesto en la posicion (",filaaleatoria,",",columnaaleatoria,") el ",matriz(filaaleatoria,columnaaleatoria)
					parar = .true.
				ENDIF
			ENDDO
			parar = .false.
			! Genero la segunda posición del número
			DO WHILE (parar .eqv. .false. )
				filaaleatoria = aleatorioEntre(1, filas+1)				! Genero una posición aleatoria para la fila
				columnaaleatoria = aleatorioEntre(1, columnas+1)		! Genero una posición aleatoria para la columna
				IF (matriz(filaaleatoria,columnaaleatoria) == -1) THEN	! Si la posición está vacía (vale -1)
					matriz(filaaleatoria,columnaaleatoria) = i			! Coloco ahí ese número
					!print*, "He puesto en la posicion (",filaaleatoria,",",columnaaleatoria,") el ",matriz(filaaleatoria,columnaaleatoria)
					parar = .true.
				ENDIF
			ENDDO
		ENDDO
		
	END SUBROUTINE generarTableroJuego

	!*****************************************
	!** Muestra una matriz por pantalla     **
	!** Parámetros:                         **
	!** 	mamtriz: matriz                 **
	!** 	filas: filas de la matriz       **
	!** 	columnas: columnas de la matriz **
	!*****************************************

	SUBROUTINE mostrarMatriz(matriz, filas, columnas)
		INTEGER, DIMENSION(:,:), INTENT(IN) :: matriz
		INTEGER, INTENT(IN) :: filas, columnas
		INTEGER :: i, j

		DO i = 1,filas !DO i = 1,filas
			PRINT*, (matriz(i,j), j = 1, columnas) ! bucle implícito
		ENDDO

	END SUBROUTINE mostrarMatriz
	
	!**************************************************************************
	!** Devuelve un número aleatorio entre dos números   	                 **
	!** Parámetros:                        					                 **
	!** 	limiteinferior : límite inferior de generación de los aleatorios **
	!** 	limitesuperior : límite superior de generación de los aleatorios **
	!** Valor devuelto:                    									 **
	!** 	Aleatorio entre limiteinferior y limitesuperior                  **
	!**************************************************************************

	INTEGER FUNCTION aleatorioEntre(limiteinferior, limitesuperior)
		INTEGER, INTENT(IN) :: limiteinferior, limitesuperior

		aleatorioEntre = rand()*(limitesuperior-limiteinferior) + limiteinferior

	END FUNCTION aleatorioEntre
	
end program memory

