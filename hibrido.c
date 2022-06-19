#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include "arbol.h"
#include <mpi.h>
#include <stddef.h>
#include <omp.h>

//dimension de la matriz
#define FILAS 1200      //1500, 5000 y 7500 funciona hasta 6600
#define COLUMNAS 1200   //1500, 5000 y 7500
#define VUELTAS 5

//simulacion
#define SEMANAS 240     //240, 1200 y 4320

//imprimo los colores para mostrarlos
void mostrarColores(Celda c){
    if (c.estado == BLANCO){     //podado
        printf(" BLANCO ");
    }
    if (c.estado == AZUL){   //enfermo con tratamiento antifúngico
        printf(" AZUL ");
    }
    if (c.estado == ROJO){   //enfermo con síntomas visibles
        printf(" ROJO ");
    }
    if (c.estado == NARANJA){    //infectado con esporas (Enfermo sin síntomas)
        printf(" NARANJA ");
    }
    if (c.estado == VERDE){      //sano
        printf(" VERDE ");
    }
}

void getCeldasInfectadas(Celda c, Celda *buffer_fila, Celda *buffer_fila_sup, Celda *buffer_fila_arriba, Celda *buffer_fila_abajo,Celda *buffer_fila_inf, int cant_Vecinos, int frontera_fila, int frontera_columna, int *datos_vecino){
    int vecinos_infectados = 0;
    int vecinos = 0;

    Celda vecino[12] = {
        buffer_fila_sup[c.cord_y],
        buffer_fila_arriba[c.cord_y - 1],
        buffer_fila_arriba[c.cord_y],
        buffer_fila_arriba[c.cord_y + 1],
        buffer_fila[c.cord_y - 2],
        buffer_fila[c.cord_y - 1],
        buffer_fila[c.cord_y + 1],
        buffer_fila[c.cord_y + 2],
        buffer_fila_abajo[c.cord_y - 1],
        buffer_fila_abajo[c.cord_y],
        buffer_fila_abajo[c.cord_y + 1],
        buffer_fila_inf[c.cord_y]
        };

    for (int i = 0; i < cant_Vecinos; i ++){ //se suma 2 por el arreglo de vecinos
        if (vecino[i].cord_y == 0 || vecino[i].cord_y == frontera_fila || vecino[i].cord_y == frontera_columna ||
            vecino[i].cord_y == 1 || vecino[i].cord_y == frontera_fila-1 || vecino[i].cord_y == frontera_columna-1){
                //caso fuera de matriz
        }
        else{
            vecinos++;
            if (vecino[i].estado == ROJO){  //si el vecino es ROJO (enfermo) sumo 1 a vecinos enfermos
                vecinos_infectados += 1;
            }
        }
    }
   // printf("Vecinos en [%d] [%d] enfermos =  %d\n", c.cord_x, c.cord_y, vecinos_enfermos);
    datos_vecino[0] = vecinos;
    datos_vecino[1] = vecinos_infectados;
}

float EvaluarResistencia(Celda c){
    float resistencia;
    if (c.edad < 157){
        resistencia = 3;
    }
    else if (c.edad >= 157 && c.edad <= 1820){
        resistencia = 15;
    }
    else if (c.edad > 1820){
        resistencia = 53;
    }
    return resistencia;
}

void initMatrix(int filas, int columnas, Celda *tablero){
    srand(time(NULL));
    for (int i = 0; i < filas; i++){
        for (int j = 0; j < columnas; j++){
            int k = (i * columnas) + j; // k = (index de la fila * cantidad de columnas) + indice de la columna.  Cantidad de columnas= largo de la fila.
            int random_edad = rand() % 101; //random entre 1 y 100
            int random_colores = rand() % 101; //random entre 1 y 100
            int random_heridas = rand() % 101; //random entre 1 y 100
            int color = -1;
            int edad;
            int init_timer = 0;
            int heridas_abiertas = 0;   //0 = sin heridas

            //frontera con -1
            if (i == 0 || j == 0 || i == filas - 1 || j == columnas - 1 || i == 1 || j == 1 || i == filas - 2 || j == columnas - 2){
                edad = -1;
                color = -1;
                tablero[k].cord_x = i;
                tablero[k].cord_y = j;
                tablero[k].edad = edad;
                tablero[k].estado = color;
            }
            else{
                //color
                if (random_colores < 65){   //65% de árboles sanos
                    color = VERDE;
                }
                if (random_colores >= 65 && random_colores < 70){   //5% de árboles con síntomas
                    color = ROJO;
                    init_timer = 4; //enfermo con sintomas en el tiempo 4
                }
                if (random_colores >= 70 && random_colores < 80){   //10% de árboles enfermos sin síntomas
                    color = NARANJA;
                    init_timer = 1; //enfermo sin sintomas en el tiempo 1
                }
                if (random_colores >= 80 && random_colores <= 100){   //20% de árboles con tratamiento
                    color = AZUL;
                    init_timer = 5; //enfermo con tratamiento en el tiempo 5
                }

                //edad
                if (random_edad < 30){   //30% jovenes
                    edad = 100;
                }
                else if (random_edad >= 30 && random_edad < 80){   //50% adultos
                    edad = 1000;
                }
                else{   //20% viejos
                    edad = 1900;
                }

                //heridas abiertas
                if (random_heridas < 23 && edad<157){   //23% en jovenes
                    heridas_abiertas = 1;
                }
                else if(random_heridas < 8 && edad>=157 && edad <1821){   //8% en adultos
                    heridas_abiertas = 1;
                }
                else if(random_heridas < 37 && edad>=1821){   //37% en viejos
                    heridas_abiertas = 1;
                }

                //crear celda
                tablero[k].cord_x = i;
                tablero[k].cord_y = j;
                tablero[k].edad = edad;
                tablero[k].estado = color;
                tablero[k].heridas_abiertas = heridas_abiertas;
                tablero[k].timer = init_timer;
            }
        }
    }
}

void Calcular_Proximo_Estado(Celda *tablero, Celda *guardo_resultado, Celda c, Celda *busqueda_fila_actual, Celda *buffer_fila_sup, Celda *buffer_fila_arriba, Celda *buffer_fila_abajo, Celda *buffer_fila_inf, int frontera_fila, int frontera_columna){
    int prob_tratamiento = 0;
    float resistencia = 0;
    float prob_contagio = 0;
    int datos_vecinos[2]; //arreglo con la cantida de vecinos y la cantidad de enfermos

    //evaluar estado de celda
    if ((c).estado == VERDE){
        //arbol Sano -> Enfermo sin sintomas
        getCeldasInfectadas(c, busqueda_fila_actual, buffer_fila_sup,buffer_fila_arriba,buffer_fila_abajo, buffer_fila_inf, VECINOS, frontera_fila, frontera_columna, &datos_vecinos);

        if (datos_vecinos[1] == 0){    //no hay vecinos enfermos
            /*prob_contagio = 0;
            c.estado = VERDE;
            c.timer = 0;*/
            int randomEnfermo = (rand()%1001)/1000.0f;
            prob_contagio = sano_enf_sin_sintomas(c, datos_vecinos[0], datos_vecinos[1]);
            if (randomEnfermo <= prob_contagio){
                c.estado = NARANJA; //enfermo sin sintomas
            }
            else{
                prob_contagio = 0;
                c.estado = VERDE;
                c.timer = 0;
            }
        }
        else{   //hay vecinos enfermos
            prob_contagio = sano_enf_sin_sintomas(c, datos_vecinos[0], datos_vecinos[1]);
            int randomEnfermo = (rand()%1001)/1000.0f;
            if (randomEnfermo <= prob_contagio){
                c.estado = NARANJA; //enfermo sin sintomas
            }
        }
    }
    else if ((c).estado == NARANJA){
        // Infectado con esporas -> Enfermo con síntomas
        if ((c).timer == 4){   //pasan 3 semanas del contagio
            c.estado = ROJO;
        }
    }
    else if ((c).estado == ROJO){
        // Enfermo con síntomas -> Enfermo con tratamiento antifúngico
            prob_tratamiento = rand() % 100;
            if (prob_tratamiento <= 85){
                c.estado = AZUL;
            }
    }
    else if ((c).estado == AZUL){
        // Enfermo con tratamiento antifúngico -> Recuperado/Podado/Reemplazado:
        if ((c).timer == 9){    //pasan 8 semanas desde el contagio
            resistencia = EvaluarResistencia(c);
            if(c.edad<157){
                if(rand()%100 > resistencia){
                    c.estado = VERDE;
                    c.timer=0;
                }
                else{
                    c.estado = BLANCO;  //podado
                }
            }
            if(c.edad>=157 && c.edad<1821){
                if(rand()%100 > resistencia){
                    c.estado = VERDE;
                    c.timer = 0;
                }
                else{
                    c.estado = BLANCO;  //podado
                }
            }
            if(c.edad>=1821){
                if(rand()%100 > resistencia){
                    c.estado = VERDE;
                    c.timer = 0;
                }
                else{
                    c.estado = VERDE;   //reemplazo por joven sano de un año
                    c.edad = 52;
                    c.timer = 0;
                }
            }
        }
    }
    else if((c).estado == BLANCO){
        //arbol Podado -> arbol Sano
        if ((c).timer == 21){     //pasan 12 semanas de la poda
            c.estado = VERDE;
            c.timer = 0;
        }
    }
    else{
        //nada
    }
    c.timer++;  //aumento tiempo
    c.edad++;   //aumento edad
    guardo_resultado[(c).cord_y] = c;
}

void getSubMatrix(Celda *matrix, Celda *submatrix, int start, int finish){
    //obtengo la parte de la matriz para enviar
    for (int i = start; i < finish; i++){
        submatrix[i - start] = matrix[i];   //i-start para que arranque en 0
        //printf("%d ", i);
    }
}

int main(int argc, char **argv){
    //frontera
    int frontera_filas = FILAS + 4;
    int frontera_columnas = COLUMNAS + 4;
    int start; //flag para desplazamiento
    int retorno = 0;
    int control_iteraciones; //controla la cantidad de iteraciones para evitar overflow

    //memoria para la matriz original
    Celda *tablero;
    Celda *tablero_auxiliar;
    Celda *temp;

    //paralelismo
    Celda *busqueda_fila_superior;  //la de mas arriba
    Celda *busqueda_fila_inferior;  //la de mas abajo
    Celda *busqueda_fila_actual;
    Celda *busqueda_fila_arriba;    //la vecina de arriba
    Celda *busqueda_fila_abajo;     //la vecina de abajo
    Celda *submatrix;
    Celda *guardo_resultado;
    Celda *guardo_resultado_auxiliar;

    //memoria para la matriz original
    tablero = (Celda *)malloc((frontera_filas * frontera_columnas) * sizeof(Celda));
    if (tablero == NULL){
        printf("No se ha podido reservar memoria");
    }
    //memoria para obtener filas vecinas
    busqueda_fila_actual = (Celda *)malloc(frontera_columnas * sizeof(Celda));
    if (busqueda_fila_actual == NULL){
        printf("No se ha podido reservar memoria");
    }
    busqueda_fila_inferior = (Celda *)malloc(frontera_columnas * sizeof(Celda));
    if (busqueda_fila_inferior == NULL){
        printf("No se ha podido reservar memoria");
    }

    busqueda_fila_abajo = (Celda *)malloc(frontera_columnas * sizeof(Celda));
    if (busqueda_fila_abajo == NULL){
        printf("No se ha podido reservar memoria");
    }

    busqueda_fila_superior = (Celda *)malloc(frontera_columnas * sizeof(Celda));
    if (busqueda_fila_superior == NULL){
        printf("No se ha podido reservar memoria");
    }

    busqueda_fila_arriba = (Celda *)malloc(frontera_columnas * sizeof(Celda));
    if (busqueda_fila_arriba == NULL){
        printf("No se ha podido reservar memoria");
    }

    guardo_resultado = (Celda *)malloc(frontera_columnas * sizeof(Celda));
    if (guardo_resultado == NULL){
        printf("No se ha podido reservar memoria");
    }

   //MPI

    MPI_Init(&argc, &argv);
    MPI_Status status;

    //obtengo el tamaño de procesos
    int world_size;
    MPI_Comm_size(MPI_COMM_WORLD, &world_size);

    //obtengo el rango de los procesos
    int world_rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &world_rank);

    //tipo de dato personalizado MPI
    MPI_Datatype MPI_CELDA;
    int mpi_celda_block_lengths[] = {1, 1, 1, 1, 1, 1};
    MPI_Aint mpi_celda_displacements[] = {
        offsetof(Celda, cord_x),
        offsetof(Celda, cord_y),
        offsetof(Celda, estado),
        offsetof(Celda, edad),
        offsetof(Celda, heridas_abiertas),
        offsetof(Celda, timer)};
    MPI_Datatype mpi_celda_lenghts[] = {MPI_INT, MPI_INT, MPI_INT, MPI_INT, MPI_INT, MPI_INT};

    int elementos_a_enviar = frontera_columnas * world_size; //cantidad de elementos a enviar en Scatter
    int total_values = frontera_filas * frontera_columnas;

    MPI_Type_create_struct(
        6,
        mpi_celda_block_lengths,
        mpi_celda_displacements,
        mpi_celda_lenghts,
        &MPI_CELDA);
    MPI_Type_commit(&MPI_CELDA);

    //inicializo valores
    if (world_rank == 0){
        //inicializo matriz
        initMatrix(frontera_filas, frontera_columnas, tablero);
        start = 0;

        //memoria para la matriz auxiliar (guardo estados anteriores)
        tablero_auxiliar = (Celda *)malloc((frontera_filas * frontera_columnas) * sizeof(Celda));
        if (tablero_auxiliar == NULL){
            printf("Falla en reserva de memoria.");
        }

        guardo_resultado_auxiliar = (Celda *)malloc(elementos_a_enviar * sizeof(Celda));
        if (guardo_resultado == NULL){
            printf("Falla en reserva de memoria.");
        }
    }
    //SIMULACION
    struct timeval tiempo_inicial, tiempo_final;
    double tiempo;
    double tiempo_total = 0;
    double tiempo_promedio = 0;
    int cuentoSemanas = 0;
    int cuentoVueltas = 1;

    submatrix = (Celda *)malloc(elementos_a_enviar * sizeof(Celda));
    if (submatrix == NULL){
        printf("Falla en reserva de memoria.");
    }

    for (int i = 0; i < VUELTAS; i++){
        if (world_rank == 0){
            gettimeofday(&tiempo_inicial, NULL); //tiempo inicial
        }
        printf("\n------------ VUELTA %d ------------\n\n", cuentoVueltas);
        for (int dias = 0; dias < SEMANAS; dias++){
            //divido la matriz para cada proceso
            for (int i = frontera_columnas; i < total_values; i += elementos_a_enviar){
                if (world_rank == 0){
                    getSubMatrix(tablero, submatrix, i + frontera_columnas, (i + frontera_columnas) + elementos_a_enviar); //obtengo las filas
                }

                MPI_Scatter(submatrix, frontera_columnas, MPI_CELDA, busqueda_fila_actual, frontera_columnas, MPI_CELDA, 0, MPI_COMM_WORLD);

                if (world_rank == 0){
                    getSubMatrix(tablero, submatrix, (i - frontera_columnas), (i - frontera_columnas) + elementos_a_enviar); //obtengo las filas superiores
                }
                MPI_Scatter(submatrix, frontera_columnas, MPI_CELDA, busqueda_fila_superior, frontera_columnas, MPI_CELDA, 0, MPI_COMM_WORLD);

                if (world_rank == 0){
                    getSubMatrix(tablero, submatrix, i, (i + elementos_a_enviar)); //obtengo las filas de arriba
                }
                MPI_Scatter(submatrix, frontera_columnas, MPI_CELDA, busqueda_fila_arriba, frontera_columnas, MPI_CELDA, 0, MPI_COMM_WORLD);

                if (world_rank == 0){
                    getSubMatrix(tablero, submatrix, (i + (2*frontera_columnas)), (i + (2*frontera_columnas)) + elementos_a_enviar); //obtengo las filas de abajo
                }
                MPI_Scatter(submatrix, frontera_columnas, MPI_CELDA, busqueda_fila_inferior, frontera_columnas, MPI_CELDA, 0, MPI_COMM_WORLD);

                if (world_rank == 0){
                    getSubMatrix(tablero, submatrix, (i + (3*frontera_columnas)), (i + (3*frontera_columnas)) + elementos_a_enviar); //obtengo las inferiores
                }
                MPI_Scatter(submatrix, frontera_columnas, MPI_CELDA, busqueda_fila_inferior, frontera_columnas, MPI_CELDA, 0, MPI_COMM_WORLD);


                if ((busqueda_fila_actual[0].cord_x > 1) && (busqueda_fila_actual[0].cord_x < frontera_filas - 2)){
                    //actualizo los estados de las celdas si x esta en [2,filas-2] (bordes)
                    #pragma omp parallel for private(i) num_threads(4)
                    for (int i = 0; i < frontera_columnas; i++){
                        Calcular_Proximo_Estado(tablero, guardo_resultado, busqueda_fila_actual[i], busqueda_fila_actual, busqueda_fila_superior, busqueda_fila_arriba, busqueda_fila_abajo,busqueda_fila_inferior, frontera_filas, frontera_columnas);
                    }
                }
                else{
                    for (int i = 0; i < frontera_columnas; i++){
                        busqueda_fila_actual[i].estado = -1;
                        guardo_resultado[i] = busqueda_fila_actual[i];
                    }
                }

                MPI_Gather(guardo_resultado, frontera_columnas, MPI_CELDA, guardo_resultado_auxiliar, frontera_columnas, MPI_CELDA, 0, MPI_COMM_WORLD);

                if(world_rank == 0){
                    //escribo lo que tiene el buffer en la porcion de la matriz auxiliar correspondiente
                    int desplazamiento = start * elementos_a_enviar;
                    int padding = frontera_columnas; //inserto el resultado desde la 1ra fila
                    if (desplazamiento == (total_values - elementos_a_enviar)){
                        padding = 0;
                    }
                    for (int i = 0; i < elementos_a_enviar; i++){
                        tablero_auxiliar[i + desplazamiento + padding] = guardo_resultado_auxiliar[i];
                    }
                    start++;
                }
            }
            if (world_rank == 0){
                //swap punteros de la matriz
                temp = tablero;
                tablero = tablero_auxiliar;
                tablero_auxiliar = temp;

                start = 0;

            }

            /*
            printf("\n TABLERO:\n");
                 for (int i = 0; i < frontera_filas; i++){
                     for (int j = 0; j < frontera_columnas; j++){
                         int k = (i * frontera_columnas)+j;
                         if(tablero[k].cord_x >1 && tablero[k].cord_x<frontera_columnas-2 && tablero[k].cord_y >1 && tablero[k].cord_y<frontera_columnas-2){
                            printf("Pos. X: %d ", tablero[k].cord_x);
                            printf("/ Pos. Y: %d ", tablero[k].cord_y);
                            printf("/ Edad: %d", tablero[k].edad);
                            printf("/ Estado: ");
                            mostrarColores(tablero[k]);
                            printf("/ Heridas abiertas: %d", tablero[k].heridas_abiertas);
                            printf("/ Tiempo: %d ", tablero[k].timer);
                            printf("\n");
                         }
                     }
                     printf("\n");
                 }
            */
        }
        if (world_rank == 0){
            //tv_sec -> numero entero de segundos en el intervalo
            //tv_usec -> fracción adicional como microsegundos
            gettimeofday(&tiempo_final, NULL);
            tiempo = (tiempo_final.tv_sec - tiempo_inicial.tv_sec) * 1000.0 + (tiempo_final.tv_usec - tiempo_inicial.tv_usec) / 1000.0;
            printf("Tiempo vuelta: %g milisegundos\n\n", tiempo);
            tiempo_total += tiempo;
            tiempo = 0;
            cuentoVueltas++;

            initMatrix(frontera_filas, frontera_columnas, tablero);
            start = 0;
        }

    }
    if (world_rank == 0){
        tiempo_promedio = tiempo_total / VUELTAS;

        printf("\n\n--------------- INFORME DE SIMULACION ---------------\n");
        printf("\n -Dimension de matriz: %d x %d\n", FILAS, COLUMNAS);
        printf(" -Total de semanas: %d\n", SEMANAS);
        printf(" -Tiempo promedio en %d corridas: %g milisegundos \n", VUELTAS,tiempo_promedio);

        printf("\n-----------------------------------------------------\n");
    }
    MPI_Finalize();
}

