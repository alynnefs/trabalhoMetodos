#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <locale.h>
#define EPSILON 0.00000001


// Alocação, leitura e escrita de matrizes

double **alocaMatriz(int l, int c) { // aloca espaço na memória para a alocação da matriz
// Se houver espaço: aloca dinamicamente uma matriz[l][c] (sendo "l" linhas e "c" colunas), devolvendo um ponteiro para double.
// Caso contrário: devolve nulo
    double **m; int i,j; 
    m = malloc(sizeof(double *)*c);

    if(m == NULL) return NULL; // falta de memória
    for(i=0;i<c;i++) {
        m[i] = malloc(sizeof(double)*l);
        if(m[i] == NULL) { // falta de memória
            for(j = 0; j < i; j++) free(m[j]);
            free(m);
            return NULL;
        };
    }
    return m;
}

void imprimeMatriz(double **m,int l, int c) { // mostra a matriz
    int i,j;
    for(i=0;i<l;i++) {
        for(j=0;j<c;j++) {
            printf("%6.2lf ",m[j][i]);
        }
        printf("\n");
    }
}

double **leArquivo(double **m, int *nvar, FILE *fp) { // lê os dados de um arquivo
    int i, j;
    m = alocaMatriz(nvar[0],nvar[0]+1);
    
    for(j=0; j<nvar[0]; j++) {
        for(i=0; i<nvar[0]+1; i++) {
            fscanf(fp,"%lf",&m[i][j]);
    	}
    }
    
    fclose (fp);
    return m;
}


// Conversão de bases sistemas de numeração

char *giraString (char *k, int i){ // ESTA FUNÇÃO É UTILIZADA PARA INVERTER A ORDEM DA STRING UTILIZADA NA CONVERSÃO DA PARTE INTEIRA DO NÚMERO DECIMAL
    int n, t; // n É O LIMITE DO LAÇO DA INVERSÃO
    char swap;

    if(i%2 ==0) n=i/2;
    else n = (i-1)/2;
	
    for(t=0; t<n; t++){ // LAÇO DA INVERSÃO DA STRING
        swap = k[t];
        k[t] = k[i-1-t];
        k[i-1-t] = swap;
    }
    return k;
}

void converteNum(double num,int base){ // ESTA FUNÇÃO CONVERTE UM NUMÉRO DECIMAL num UM SISTEMA DE BASE k. NO PROGRAMA PRINCIPAL, AS BASES SOLICITADAS SÃO 2 (BINÁRIO), 8 (OCTAL) E 16 (HEXADECIMAL).

    int bitsConvertidos=0, casasDecimais, inteiro, bit, sinal=0; //t É O LIMITADOR DE DE CASAS DECIMAIS;
    double fracao;
    char *p;
    
    if (num < 0) {
        num = fab(num); // deixa positivo
        sinal = 1;
    }
    inteiro = abs(num); // inteiro É A PARTE INTEIRA DE num
    fracao = num-inteiro; // fracao É A PARTE FRACIONÁRIA DE num

    p = malloc(sizeof(char *)*30);
    while(inteiro !=0){ // LAÇO DE CONVERSÃO DA PARTE INTEIRA
        bit = inteiro%base;
        if(bit > 9) bit += 7;
        p[bitsConvertidos] = bit + 48;
        inteiro/=base;
        bitsConvertidos++; //QUANTOS BITS JÁ FORAM CONVERTIDOS
    }
    p = giraString(p,bitsConvertidos); // A STRING p É MONTADA NA ORDEM EM QUE OS BITS SÃO OBTIDOS, PORÉM, ELES DEVEM SER LIDOS DA FORMA INVERSA. POR ISSO, A NECESSIDADE DA INVERSÃO DE p.
    p[bitsConvertidos] = 46; // UMA VEZ QUE A PARTE INTEIRA É FINALIZADA, INSERIMOS UM PONTO (. NO CÓDIGO ASCII É 46).
    bitsConvertidos++;
    casasDecimais= bitsConvertidos + 19; // t FIXA O MÁXIMO DE 20 CASAS DECIMAIS A SEREM CONVERTIDAS A PARTIR DE i
    while(bitsConvertidos <= casasDecimais){
        fracao = fracao*base;
        bit = abs(fracao);
        if(bit > 9) bit += 7;
        p[bitsConvertidos] = bit + 48;
        if(bit >= 17) bit -= 7;
        fracao -= bit;
        bitsConvertidos++;
        if(fracao == 0) break;
    }
    p[bitsConvertidos] = '\0'; // CARACTERE NULO INSERIDO PARA DELIMITAR O FIM DA CONVERSÃO
	
    switch(base){
        case 2:
            if (sinal == 0) printf("\n%lf em binario: %s",num,p);
            else printf ("\n-%lf em binario: -%s",num,p);
            break;
        case 8:
            if (sinal == 0) printf("\n%lf em octal: %s",num,p);
            else printf("\n-%lf em octal: -%s",num,p);
            break;
        case 16:
            if (sinal == 0) printf("\n%lf em hexadecimal: %s",num,p);
            else printf("\n-%lf em hexadecimal: -%s",num,p);
            break;
    }
}


// Resolução de sistemas lineares

int achaSolucao(double **m,int n,double *x){
    int i,j,tipo=0;
	
    for(i=0;i<n;i++){
        if(m[i][i]==0){
            if(m[n][i]==0){
            /* SE O ELEMENTO DA DIAGONAL PRINCIPAL DA LINHA i NA MATRIZ DE COEFICIENTES E O TERMO INDEPENDENTE FOREM IGUAIS A 0,
            O SISTEMA É INDETERMINADO, POIS PARA QUALQUER VALOR DE X[i], A EQUAÇÃO SERÁ ATENDIDA. A SOLUÇÃO EXIBIDA, CONSIDERARÁ
            X[i] IGUAL A 0. */
                tipo=1;
                x[i]=0;	
            }
            else {
                /* SE O ELEMENTO DA DIAGONAL PRINCIPAL DA LINHA i NA MATRIZ DE COEFICIENTES FOR IGUAL A 0 E O TERMO INDEPENDENTE FOR
                DIFERENTE DE 0, O SISTEMA É IMPOSSÍVEL, POIS NÃO HÁ QUALQUER VALOR DE X[i] QUE ATENDA A ESTA CONDIÇÃO. */
                return 2;
            }
        }
        else {
            x[i]=m[n][i]/m[i][i]; // CASO HAJA SOLUÇÃO, ESTÁ SE DÁ PELA DIVISÃO DO TERMO INDEPENDENTE PELO COEFICIENTE DA LINHA i DA DIAGONAL PRINCIPAL
        }
	}
	return tipo;
}

void jordan(double **m,int n,int *seqx){ // RESOLVE UM SISTEMA LINEAR DE n VARIÁVEIS E n EQUAÇÕES PELO MÉTODO DE JORDAN

    /* ESTA FUNÇÃO RESOLVE UM SISTEMA LINEAR CUJOS COEFICIENTES FORAM INCLUIDOS
    NA MATRIZ m DE n VARIAVEIS E n EQUAÇÕES	PELO MÉTODO DE JORDAN */
    int i,j,k,t,swapX,tipo;
    double *aux, *x, mult;
	
    x=malloc(sizeof(double *)*n); // x É O VETOR DE SOLUÇÕES DO SISTEMA LINEAR, CASO HAJA
	
    printf("\n");
	
    for(i=0; i<n; i++) {
        if(fabs(m[i][i]) < EPSILON){ // CASO O PIVÔ SEJA 0, BUSCA EM COLUNAS A DIREITA UM VALOR NÃO NULO
            j=i;
            do {
                j++;
            } while(j<n && fabs(m[j][i])<EPSILON);

            if(j<n){ // SE ESSE VALOR FOR ENCONTRADO, É FEITA A TROCA DAS POSIÇÕES
                aux = m[i];
                m[i] = m[j];
                m[j] = aux;
				
                // PARA QUE AO FINAL DO PROCESSO, O USÁRIO SAIBA SE HOUVE OU NÃO TROCAS DE COLUNA, É FEITO UM REGISTRO
                swapX = seqx[i];
                seqx[i] = seqx[j];
                seqx[j] = swapX;
            }
            else { // CASO SEJA ENCONTRADA NA ITERAÇÃO i UMA LINHA DE ZEROS NA MATRIZ...
                for(t=0; t<n; t++) {
                    m[i][t]=0; // ...A COLUNA i DEVERÁ SER TODA ZERADA.
                }
            }
        } // fim do if
		
        if(fabs(m[i][i]) >= EPSILON) {
            for(j=0; j<n; j++) {
                if(j == i) j++;
                if(j < n)	{
                    mult = -m[i][j]/m[i][i];
                    for(k=0; k<=n; k++) {
                        m[k][j] += mult*m[k][i];
                    }
                }
            }
        } // fim do if
    } // fim do for
	
    printf("Matriz triangular: \n\n");
    imprimeMatriz(m,n,n+1);
    for(t=0; t<n; t++){
        printf("   x%d  ",seqx[t]); // É EXIBIDA A ORDEM DAS VARIÁVEIS, PARA QUE O USUÁRIO SAIBA SE HOUVE OU NÃO TROCAS DE COLUNA
    }
	
    printf("\n-----------------------------\n");
    printf("Resultado:\n\n");
    tipo=achaSolucao(m,n,x);
    if(tipo == 2) printf("Sistema linear impossivel");
    else {
        for(i=0; i<n; i++) {
            printf("x[%d]: %6.2lf\n",seqx[i],x[i]);
        }
        if(tipo==0) printf("\nSL determinado");
        else printf("\nSL indeterminado");
    }
}

void resolveSL() { // RESOLVE UM SISTEMA LINEAR CUJOS COEFICIENTES ESTÃO EM UM ARQUIVO .TXT INDICADO PELO USUÁRIO

    int *nvar,*seqx,tipo, t;
    char *nomeDoArquivo;
    double **m;
    FILE *fp;
    
    nomeDoArquivo = malloc(80);
    
    printf("Digite o caminho para o arquivo de texto que contem o sistema linear: \n");

    do{ fgets(nomeDoArquivo, 80, stdin); } while(nomeDoArquivo[0] == '\n'); // RECEBE O NOME DO ARQUIVO E SUA EXTENSÃO


    nomeDoArquivo[strlen(nomeDoArquivo)-1] = '\0';
    
    fp = fopen(nomeDoArquivo, "r"); // ABRE O ARQUIVO SOMENTE COMO LEITURA ("r" = READ)
//    printf("%s", fp);
    
    if (fp == NULL){
        printf("\n------ AVISO: Nao foi possivel encontrar e/ou abrir o arquivo. ------\n");
    }
    else {
        nvar = malloc(sizeof(int *));
        fscanf(fp,"%d",nvar); // LÊ O PRIMEIRO PARÂMETRO: QUANTIDADE DE VARIÁVEIS E DE EQUAÇÕES
   	 
        seqx = malloc(sizeof(int *)*nvar[0]); // CRIA UM VETOR ORDENADO DE 1 A nvar PARA AUXILIAR NA RESOLUÇÃO DO SISTEMA LINEAR
        for(t=0; t<nvar[0]; t++) {
            seqx[t]=t+1;
        }
    
        m = leArquivo(m,nvar,fp); // COLETA OS DADOS DO ARQUIVO
        printf("Matriz do arquivo: \n\n");
        imprimeMatriz(m,nvar[0],nvar[0]+1);
        jordan(m,nvar[0],seqx); // GERA A MATRIZ TRIANGULAR DO SISTEMA E PROPÕE UMA SOLUÇÃO, CASO EXISTA.	
    }
}


// Equação algébrica	
double *alocaVetor ( int l ) {
        /* Se houver memoria disponivel aloca o vetor com l colunas e devolve endereço-base da matriz;
        caso contrário devolve ponteiro nulo.*/
    int i ;
    double *v ;
    v = malloc ( l * sizeof ( double ) ) ;
    if ( v == NULL ) {/*memoria insuficiente*/
        return NULL ;
    }
    return v ;

}/*fim de alocaVetor*/

void lerVetor ( double*v, int l ) {
/*Lê valores para o vetor de double com l colunas*/

    int i ;
    for (i = l; i >= 0; i--){
        printf ("Coeficiente da variavel de grau [%d]: ", i) ;
        scanf ("%lf", &v[i]) ;
    }

}/*fim de lerVetor*/

double lagrange ( double*v , int l ) {
/* Converte o vetor, caso an seja menor que zero */
    if (v[l] < 0) {
        int i;
        for(i=l; i>=0; i--) {
            v[i] = fabs(v[i]); //* (-1) ;
        }
    }//Fim da conversao


    /*Calcula o B, K e L de Lagrange*/

    /* B - dos negativos, o maior coeficiente em modulo */
    int i, k;
    double b;
    b = (v[l]) ;
    for (i=l; i>=0; i--) {
        if(b > v[i]) {
            b = v[i];
        }
    }
    if (b > 0) {
        b = 0 ;
    }
    double ax = abs(b) ;

    /* K - maior expoente dos coeficientes negativos*/
    for (i=l; i>0; i--) {
        if (v[i] < 0) {
            k = i;
            i = 1;
        }
        else{
            k = 0;
        }
    }

    /*L - calcula o limite pelo teorema de Lagrange */
    double L, x, raiz ;
    x = v[l];
    raiz = pow( (ax/x),(1.0/ (l-k) ) ) ;
    if (raiz < 0) {
        raiz = fabs(raiz);// * ( -1 ) ;
    }
    L = 1 + (raiz) ;
    return L;

}//fim de Lagrange
    
double *inverteVetor (double*v, int l) {
/*Inverte os coeficientes do vetor*/

    int i ;
    double *inverso ;
    inverso = malloc ( l * sizeof ( double ) ) ;
    for (i=0; i<=l; i++) {
        inverso[l-i] = v[i] ;
    }
    return inverso ;

}/*fim de inverteVetor*/

double *trocaSinal ( double*v , int l ) {
/*Troca o sinal dos expoentes impares*/
    int i, resto;
    for (i=l; i>=0; i--) {
        resto = i%2;
        if (resto != 0) {
            v[i] = fabs(v[i]); //* ( -1 ) ;
        }
    }
    return v;

}/*fim de trocaSinal*/
    
void equacao(double *v, int g){
    double *Inv, lim[3];
    	
    lim[0]=lagrange(v, g);
    	
    Inv=inverteVetor(v,g);
    lim[1]=lagrange(Inv,g);
    	
    trocaSinal(v,g);
    lim[2]=lagrange(v,g);
    	
    trocaSinal(Inv,g);
    lim[3]=lagrange(Inv,g);
    
    printf ( "\n Raizes positivas [%.4lf <= E+ <= %.4lf]\n" , 1 / lim[1] ,  lim[0] ) ;
    printf ( "\n Raizes negativas [%.4lf <= E- <= %.4lf]\n" , ( -1 ) * lim[2] , ( -1 ) / lim[3] ) ;
        
    //voltar vetor ao normal
    trocaSinal(v,g);
}

void bissecao (double *v, int n){
    double a,b, f_a=0, f_b=0, m, f_m=0, erro;
    int i, iteracoes = 0;
    printf("Insira o início do intervalo: ");
    scanf("%lf", &a);
    printf("Insira o fim do intervalo: ");
    scanf("%lf", &b);
	
    for(i=0; i<=n; i++){
	    f_a = f_a + v[i]*(pow(a,i));
	    f_b = f_b + v[i]*(pow(b,i));

    }
	
    printf("f(a): %lf\nf(b): %lf\n",f_a,f_b);
	
    if(f_a*f_b < 0){
        printf("número ímpar de raízes.\n");
        do{  	
	    m = (a+b)/2;
            for(i=0; i<=n; i++){
                f_m = f_m + v[i]*pow(m,i);
                if (f_m == 0){
	            	printf("%lf é a raiz exata de f.\n", m);
                }
                if (f_m*f_a < 0){
                    b = m;
                    f_b = f_m;
                } else {
                    a = m;
                    f_a = f_m;
                }
                erro = fabs(a-b); //erro máximo
            } 
            printf("iterações: %d\n",iteracoes);
            iteracoes++;
			
            if(erro < EPSILON){
                iteracoes = 1000; //sair do while
            }
        } while(iteracoes < 1000 );

        printf("Raiz aproximada: %lf\n", m);
        } else {
	    printf("Não há número ímpar de raízes.\n");
        }
} //FIM Equações algébricas

int menu(void){
    char opcao;
    double numero, *V;
    int n;
    printf("\n\'C\' - Conversão, \n\'S\' - Sistema Linear, \n\'E\' - Equação Algébrica, \n\'F\' - Finalizar.\n");
    scanf("%s", &opcao);
    switch(toupper(opcao)){
        case 'C':
            printf("Conversão\n");
            printf("Digite o número decimal a ser convertido:\n");
            scanf("%lf", &numero);
            converteNum(numero,2);
            converteNum(numero,8);
            converteNum(numero,16);
        break;

        case 'S':
            printf("sistema linear\n");
            resolveSL();
        break;

        case 'E':

            printf("Digite o grau da equação: ");
            scanf ("%d", &n);
            V = alocaVetor(n) ;
            lerVetor(V , n) ;
            if (V[0] == 0){
                printf("Erro. Equação inválida.") ;
                break;
            }
            equacao(V, n) ;
            
            bissecao(V,n);
            break;

        case 'F':
            printf("finalizar\n");
            return 1;
        break;

        default:
            printf("opção inválida\n");
    }
}

int main(void){
    setlocale(LC_ALL, "Portuguese");
    int flag = 0;
    while(1){
        flag = menu(); //consertar finalização
        if (flag == 1){ return 0;}
    }
}
