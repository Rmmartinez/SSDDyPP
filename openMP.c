#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include "arbol.h"
#include <omp.h>

//dimension de la matriz
#define FILAS 2000
#define COLUMNAS 2000
#define VUELTAS 5

//simulacion
#define SEMANAS 240

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

void getCeldasInfectadas(Celda c, Celda **tablero, int cant_Vecinos, int frontera_fila, int frontera_columna, int *datos_vecino){
    int vecinos_enfermos = 0; //voy a almacenar la cantidad de vecinos enfermos (ROJOS)
    int vecinos = 0;    //cantidad de vecinos
    Celda vecino;

    Celda arr_vecino[12] = {    //obtengo vecinos
        tablero[c.cord_x + ARRIBA][c.cord_y + IZQUIERDA],
        tablero[c.cord_x + ARRIBA][c.cord_y + CENTRO],
        tablero[c.cord_x + ARRIBA][c.cord_y + DERECHA],
        tablero[c.cord_x + CENTRO][c.cord_y + IZQUIERDA],
        tablero[c.cord_x + CENTRO][c.cord_y + DERECHA],
        tablero[c.cord_x + ABAJO][c.cord_y + IZQUIERDA],
        tablero[c.cord_x + ABAJO][c.cord_y + CENTRO],
        tablero[c.cord_x + ABAJO][c.cord_y + DERECHA],
        tablero[c.cord_x + IZQUIERDA+1][c.cord_y + CENTRO],
        tablero[c.cord_x + DERECHA+1][c.cord_y + CENTRO],
        tablero[c.cord_x + CENTRO][c.cord_y + ARRIBA+1],
        tablero[c.cord_x + CENTRO][c.cord_y + ABAJO+1]
    };

    for (int i = 0; i < cant_Vecinos; i++){  //se suma 2 por el arreglo de vecinos
        int cordx_vecino = arr_vecino[i].cord_x;
        int cordy_vecino = arr_vecino[i].cord_y;

        if (cordx_vecino == 0 || cordy_vecino == 0 || cordx_vecino == frontera_fila - 1 || cordy_vecino == frontera_columna - 1 ||
            cordx_vecino == 1 || cordy_vecino == 1 || cordx_vecino == frontera_fila - 2 || cordy_vecino == frontera_columna - 2){
                //caso fuera de matriz
        }
        else{
            vecinos++;
            vecino = tablero[cordx_vecino][cordy_vecino];

            if (vecino.estado == ROJO){ //si el vecino es ROJO (enfermo) sumo 1 a vecinos enfermos
                vecinos_enfermos++;;
            }
        }
    }
    // printf("Vecinos en [%d] [%d] enfermos =  %d\n", c.cord_x, c.cord_y, vecinos_enfermos);
    datos_vecino[0] = vecinos;
    datos_vecino[1] = vecinos_enfermos;
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

void initMatrix(int filas, int columnas, Celda **tablero){
    srand(time(NULL));
    for (int i = 0; i < filas; i++){
        for (int j = 0; j < columnas; j++){
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
                tablero[i][j].cord_x = i;
                tablero[i][j].cord_y = j;
                tablero[i][j].edad = edad;
                tablero[i][j].estado = color;
            }
            else{   //matriz por dentro
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
                tablero[i][j].cord_x = i;
                tablero[i][j].cord_y = j;
                tablero[i][j].edad = edad;
                tablero[i][j].estado = color;
                tablero[i][j].heridas_abiertas = heridas_abiertas;
                tablero[i][j].timer = init_timer;
            }
        }
    }
}

void Calcular_Proximo_Estado(Celda **tablero, Celda **tablero_auxiliar, Celda c, int frontera_fila, int frontera_columna){
    int prob_tratamiento = 0;
    float resistencia = 0;
    float prob_contagio = 0;
    int datos_vecinos[2]; //arreglo con la cantida de vecinos y la cantidad de enfermos


    //Todo esto es critical

    //evaluar estado de celda
    if ((c).estado == VERDE){
        //arbol Sano -> Enfermo sin sintomas
        getCeldasInfectadas(c, tablero, VECINOS, frontera_fila, frontera_columna, &datos_vecinos);

        if (datos_vecinos[1] == 0){    //no hay vecinos enfermos
            prob_contagio = 0;
            c.estado = VERDE;
            c.timer = 0;
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
                if(rand()%100 <= resistencia){
                    c.estado = VERDE;
                    c.timer=0;
                }
                else{
                    c.estado = BLANCO;  //podado
                }
            }
            if(c.edad>=157 && c.edad<1821){
                if(rand()%100 <= resistencia){
                    c.estado = VERDE;
                    c.timer = 0;
                }
                else{
                    c.estado = BLANCO;  //podado
                }
            }
            if(c.edad>=1821){
                if(rand()%100 <= resistencia){
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
    tablero_auxiliar[(c).cord_x][(c).cord_y] = c;
}

int main(){
    //frontera
    int frontera_filas = FILAS + 4;         //4 para bordes
    int frontera_columnas = COLUMNAS + 4;   //4 para bordes

    //memoria para la matriz original
    Celda **tablero;
    tablero = (Celda **)malloc(sizeof(Celda *) * frontera_filas);
    if (tablero == NULL){
        printf("Falla en reserva de memoria.");
    }

    for (int i = 0; i < frontera_filas; i++){
        tablero[i] = (Celda *)malloc(sizeof(Celda) * frontera_columnas);
        if (tablero[i] == NULL){
            printf("Falla en reserva de memoria.");
        }
    }

    //memoria para la matriz auxiliar (guardo estados anteriores)
    Celda **tablero_auxiliar;
    tablero_auxiliar = (Celda **)malloc(sizeof(Celda *) * frontera_filas);
    if (tablero_auxiliar == NULL){
    }
    for (int i = 0; i < frontera_filas; i++){
        tablero_auxiliar[i] = (Celda *)malloc(sizeof(Celda) * frontera_columnas);
        if (tablero_auxiliar[i] == NULL){
        }
    }

    //inicializo matriz
    initMatrix(frontera_filas, frontera_columnas, tablero);

    //copio tablero a tablero_auxiliar
    for (int i = 0; i < frontera_filas; i++){
        for (int j = 0; j < frontera_columnas; j++){
            tablero_auxiliar[i][j] = tablero[i][j];
        }
    }
    Celda **temp; //puntero temporal para hacer swap

    // SIMULACION
    struct timeval tiempo_inicial, tiempo_final;
    double tiempo;
    double tiempo_total = 0;
    double tiempo_promedio = 0;
    int cuentoSemanas = 0;
    int cuentoVueltas = 1;
    for (int i = 0; i < VUELTAS; i++){
        //inicializo matriz
        initMatrix(frontera_filas, frontera_columnas, tablero);
        gettimeofday(&tiempo_inicial, NULL); //tiempo inicial
        printf(" \n-------------------\n");
        printf(" VUELTA: %d\n", cuentoVueltas);
        for(int i = 0; i < SEMANAS; i++){
            //actualizo proximos estados de la matriz
            #pragma omp parallel for private(i) collapse(2) num_threads(8)
            for (int i = 2; i < frontera_filas - 2; i++){
                for (int j = 2; j < frontera_columnas - 2; j++){
                    Calcular_Proximo_Estado(tablero, tablero_auxiliar, tablero[i][j], frontera_filas, frontera_columnas);
                }
            }

            //swap punteros de la matriz
            temp = tablero;
            tablero = tablero_auxiliar;
            tablero_auxiliar = temp;

            //valores de la matriz
            /*
             printf("\n TABLERO SEMANA: %d\n", i+1);
             printf("-------------------\n");
             for(int i = 2; i < frontera_filas-2; i++){
                 for (int j = 2; j < frontera_columnas-2; j++){
                     printf("Pos. X: %d ", tablero[i][j].cord_x);
                     printf("/ Pos. Y: %d ", tablero[i][j].cord_y);
                     printf("/ Edad: %d ", tablero[i][j].edad);
                     printf("/ Estado: ");
                     mostrarColores(tablero[i][j]);
                     printf("/ Heridas abiertas: %d ", tablero[i][j].heridas_abiertas);
                     printf("/ Tiempo: %d ", tablero[i][j].timer);
                     printf("\n");
                 }
                 printf("\n");
             }
             */
        }
        gettimeofday(&tiempo_final, NULL);
        //tv_sec -> numero entero de segundos en el intervalo
        //tv_usec -> fracción adicional como microsegundos
        tiempo = (tiempo_final.tv_sec - tiempo_inicial.tv_sec) * 1000.0 + (tiempo_final.tv_usec - tiempo_inicial.tv_usec) / 1000.0;
        printf("TIEMPO VUELTA: %g milisegundos\n", tiempo);

            tiempo_total += tiempo;
            tiempo = 0;
            cuentoVueltas++;
    }
    tiempo_promedio = tiempo_total / VUELTAS;

    printf("\n\n--------------- INFORME DE SIMULACION ---------------\n");
    printf("\n -Dimension de matriz: %d x %d\n", FILAS, COLUMNAS);
    printf(" -Total de semanas: %d\n", SEMANAS);
    printf(" -Tiempo promedio en %d corridas: %g milisegundos \n", VUELTAS,tiempo_promedio);
    printf("\n---------------------------------------------------- \n");

    return 0;
}
