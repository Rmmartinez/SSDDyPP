//colores
#define BLANCO 0   // podado
#define AZUL 1     // enfermo con tratamiento antifúngico
#define NARANJA 2  // infectado con esporas (Enfermo sin síntomas)
#define ROJO 3     // enfermo con síntomas visibles
#define VERDE 4    // sano

//para vecinos
#define IZQUIERDA -1
#define DERECHA 1
#define CENTRO 0
#define ARRIBA -1
#define ABAJO 1

#define VECINOS 12

typedef struct Celda{
    int cord_x;
    int cord_y;
    int estado;             // color
    int edad;               // joven (hasta 3 años (hasta semana 156)),adulto (4 a 35 años (semana 157 a 1820)),viejo(mas de 35 años (desde semana 1821))
    int heridas_abiertas;   // Flag -> 1 si, 0 no
    int timer;              // paso de tiempo
} Celda;


//funcion sano -> enfermo sin sintomas

float sano_enf_sin_sintomas(Celda Celda, int cantidad_vecinos, int cantidad_vecinos_infectados){
    float susceptibilidad = 0;
    float prob_contagio = 0;

    if (Celda.estado == AZUL){
        //calculo suceptibilidad
        if (Celda.edad < 157){    //joven
            susceptibilidad = 0.35;
            if (Celda.heridas_abiertas){
                susceptibilidad += 0.15;
            }
        }
        else if (Celda.edad >= 157 && Celda.edad <= 1820){ //adulto
            susceptibilidad = 0.17;
            if (Celda.heridas_abiertas){
                susceptibilidad += 0.15;
            }
        }
        else if (Celda.edad > 1820){  //viejo
            susceptibilidad = 0.63;
            if (Celda.heridas_abiertas){
                susceptibilidad += 0.15;
            }
        }
        //Prob. contagio= (%vecinos-con-síntomas + susceptibilidad (edad, heridas))* 0.60 + 0.07
        prob_contagio = (cantidad_vecinos_infectados + susceptibilidad)* 0.6 + 0.07;
        return prob_contagio;
    }
    return 0;
}
