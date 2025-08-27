       IDENTIFICATION DIVISION.
       PROGRAM-ID. Projeto.
       AUTHOR. Sr. Ramalho.
       DATE-WRITTEN. 2024.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

      *CONTROLE DE REPETIÇÃO
       77 CONTINUAR-PEDIDO     PIC X VALUE "S".

      *PEDIDO
       01 PEDIDO.
          05 NUMERO-PEDIDO        PIC 9(2).
          05 CLIENTE-PEDIDO       PIC X(30).
          05 CONTATO-PEDIDO       PIC X(9).
          05 DATA-PEDIDO          PIC 9(8).
          05 HORA-PEDIDO          PIC 9(6).
          05 TIPO-PEDIDO          PIC 9.

      *INGREDIENTES DO PEDIDO
       01 INGREDIENTES-PEDIDO.
          05 ITEM-ING OCCURS 5 TIMES.
             10 CODIGO-ING        PIC 9.
             10 PRECO-ING         PIC 9V99.

      *VALORES
       77 TOTAL-PEDIDO         PIC 9(2)V99.
       77 IVA-PEDIDO           PIC 9(2)V99.
       77 FINAL-PEDIDO         PIC 9(2)V99.

      *STATUS
       77 STATUS-ARQUIVO       PIC XX.

      *DATA E HORA
       01 DATA-ATUAL.
          05 ANO                  PIC 9(4).
          05 MES                  PIC 99.
          05 DIA                  PIC 99.

       01 HORA-ATUAL.
          05 HORA                 PIC 99.
          05 MINUTO               PIC 99.
          05 SEGUNDO              PIC 99.

      *CONTADORES
       77 CONTADOR-ING         PIC 9 VALUE 0.
       77 ULTIMO-PEDIDO        PIC 9(3) VALUE 0.
       77 NUM-ING              PIC 9 VALUE 0.

      *PREÇOS
       01 TABELA-PRECOS.
          05 PRECO-PIZZA OCCURS 3 TIMES.
             10 VALOR-PIZZA       PIC 9V99.

       01 TABELA-INGREDIENTES.
          05 INGREDIENTE OCCURS 10 TIMES.
             10 NOME-ING          PIC X(15).
             10 VALOR-ING         PIC 9V99.

      *SELEÇÃO DE INGREDIENTES
       01 SELECAO-INGREDIENTES.
          05 CODIGO-SEL OCCURS 5 TIMES PIC 9(2).
          05 PRECO-SEL  OCCURS 5 TIMES PIC 9V99.

      *TOTAIS
       77 TOTAL-ING-CALC       PIC 9(2)V99 VALUE 0.
       77 TOTAL-PIZZA-CALC     PIC 9(2)V99 VALUE 0.
       77 TOTAL-IVA-CALC       PIC 9(2)V99 VALUE 0.
       77 TOTAL-FINAL-CALC     PIC 9(2)V99 VALUE 0.

       77 TOTAL-ING            PIC Z9,99.
       77 TOTAL-PIZZA          PIC Z9,99.
       77 TOTAL-IVA            PIC Z9,99.
       77 TOTAL-FINAL          PIC Z9,99.

       77 STRING-DATA          PIC X(10).


       SCREEN SECTION.
      *01 LIMPAR-TELA          BLANK SCREEN.
       PROCEDURE DIVISION.
      *     DISPLAY LIMPAR-TELA.

       PARAGRAFO-PRINCIPAL.
           PERFORM INICIALIZAR-PROGRAMA
           PERFORM UNTIL CONTINUAR-PEDIDO = "N"
      *         DISPLAY LIMPAR-TELA
               PERFORM OBTER-DATA-HORA-SISTEMA
               PERFORM EXIBIR-MENU
               PERFORM PROCESSAR-PEDIDO
               DISPLAY "Deseja fazer outro pedido? (S/N): "
               ACCEPT CONTINUAR-PEDIDO
               IF CONTINUAR-PEDIDO = "S" OR CONTINUAR-PEDIDO = "s"
                   MOVE "S" TO CONTINUAR-PEDIDO
                   MOVE 0 TO NUM-ING
                   MOVE 0 TO TOTAL-ING-CALC
                   MOVE 0 TO TOTAL-PIZZA-CALC
                   MOVE 0 TO TOTAL-IVA-CALC
                   MOVE 0 TO TOTAL-FINAL-CALC
               ELSE
                   MOVE "N" TO CONTINUAR-PEDIDO
               END-IF
           END-PERFORM
           STOP RUN.

       INICIALIZAR-PROGRAMA.
           MOVE 3,00 TO VALOR-PIZZA(1)
           MOVE 4,00 TO VALOR-PIZZA(2)
           MOVE 5,00 TO VALOR-PIZZA(3)

           MOVE "Fiambre"    TO NOME-ING(1)
           MOVE 0,50         TO VALOR-ING(1)
           MOVE "Atum"       TO NOME-ING(2)
           MOVE 0,70         TO VALOR-ING(2)
           MOVE "Anchovas"   TO NOME-ING(3)
           MOVE 0,40         TO VALOR-ING(3)
           MOVE "Camarão"    TO NOME-ING(4)
           MOVE 0,80         TO VALOR-ING(4)
           MOVE "Bacon"      TO NOME-ING(5)
           MOVE 0,90         TO VALOR-ING(5)
           MOVE "Banana"     TO NOME-ING(6)
           MOVE 0,30         TO VALOR-ING(6)
           MOVE "Ananás"     TO NOME-ING(7)
           MOVE 0,40         TO VALOR-ING(7)
           MOVE "Azeitonas"  TO NOME-ING(8)
           MOVE 0,30         TO VALOR-ING(8)
           MOVE "Cogumelos"  TO NOME-ING(9)
           MOVE 0,60         TO VALOR-ING(9)
           MOVE "Milho"      TO NOME-ING(10)
           MOVE 0,50         TO VALOR-ING(10).

       OBTER-DATA-HORA-SISTEMA.
           ACCEPT DATA-ATUAL FROM DATE YYYYMMDD
           ACCEPT HORA-ATUAL FROM TIME
           MOVE SPACES TO STRING-DATA
           STRING DIA DELIMITED BY SIZE
                  "/" DELIMITED BY SIZE
                  MES DELIMITED BY SIZE
                  "/" DELIMITED BY SIZE
                  ANO DELIMITED BY SIZE
                  INTO STRING-DATA
           MOVE DATA-ATUAL TO DATA-PEDIDO
           MOVE HORA-ATUAL TO HORA-PEDIDO.

       EXIBIR-MENU.
           DISPLAY "----------------------------------------"
           DISPLAY "Pizzaria Ramalho, GestPedidosBeta-1"
           DISPLAY "Pizzas e Derivados, Lda."
           DISPLAY "Bem vindo!"
           DISPLAY "----------------------------------------"
           ADD 1 TO ULTIMO-PEDIDO
           MOVE ULTIMO-PEDIDO TO NUMERO-PEDIDO
           DISPLAY "Numero do Pedido: " NUMERO-PEDIDO
           DISPLAY "Nome do Cliente: "
           ACCEPT CLIENTE-PEDIDO
           DISPLAY "Contacto: "
           ACCEPT CONTATO-PEDIDO
           DISPLAY "Data: " STRING-DATA
           DISPLAY "Hora: " HORA ":" MINUTO ":" SEGUNDO
           DISPLAY "----------------------------------------"
           DISPLAY "[1] Pequena (3,00 EUR)"
           DISPLAY "[2] Média (4,00 EUR)"
           DISPLAY "[3] Grande (5,00 EUR)"
           DISPLAY "Escolha o tamanho da pizza: "
           ACCEPT TIPO-PEDIDO
           IF TIPO-PEDIDO < 1 OR TIPO-PEDIDO > 3
               DISPLAY "Erro: Tamanho de pizza inválido!"
               STOP RUN
           END-IF
           DISPLAY "----------------------------------------"
           DISPLAY "Ingredientes disponíveis:"
           DISPLAY "1. Fiambre (0,50 EUR)"
           DISPLAY "2. Atum (0,70 EUR)"
           DISPLAY "3. Anchovas (0,40 EUR)"
           DISPLAY "4. Camarão (0,80 EUR)"
           DISPLAY "5. Bacon (0,90 EUR)"
           DISPLAY "6. Banana (0,30 EUR)"
           DISPLAY "7. Ananás (0,40 EUR)"
           DISPLAY "8. Azeitonas (0,30 EUR)"
           DISPLAY "9. Cogumelos (0,60 EUR)"
           DISPLAY "10. Milho (0,50 EUR)"
           DISPLAY "----------------------------------------"
           DISPLAY "Quantidade de ingredientes (máximo 5): "
           ACCEPT NUM-ING
           IF NUM-ING > 5
               DISPLAY "Erro: Máximo de 5 ingredientes!"
               STOP RUN
           END-IF
           IF NUM-ING > 0
               PERFORM OBTER-INGREDIENTES
           END-IF.

       OBTER-INGREDIENTES.
           MOVE 1 TO CONTADOR-ING
           PERFORM UNTIL CONTADOR-ING > NUM-ING
               DISPLAY "----------------------------------------"
               DISPLAY "Selecione o ingrediente " CONTADOR-ING
               DISPLAY "Digite o código (1-10): "
               ACCEPT CODIGO-SEL(CONTADOR-ING)
               IF CODIGO-SEL(CONTADOR-ING) >= 1 AND
                  CODIGO-SEL(CONTADOR-ING) <= 10
                   MOVE VALOR-ING(CODIGO-SEL(CONTADOR-ING))
                        TO PRECO-SEL(CONTADOR-ING)
                   DISPLAY "Selecionado: "
                           NOME-ING(CODIGO-SEL(CONTADOR-ING))
                   DISPLAY "Preço: " PRECO-SEL(CONTADOR-ING) " EUR"
               ELSE
                   DISPLAY "Erro: Código inválido!"
                   STOP RUN
               END-IF
               ADD 1 TO CONTADOR-ING
           END-PERFORM
           DISPLAY "----------------------------------------"
           DISPLAY "Ingredientes selecionados:"
           MOVE 1 TO CONTADOR-ING
           PERFORM UNTIL CONTADOR-ING > NUM-ING
               DISPLAY CONTADOR-ING ". "
                       NOME-ING(CODIGO-SEL(CONTADOR-ING))
                       " - " PRECO-SEL(CONTADOR-ING) " EUR"
               ADD 1 TO CONTADOR-ING
           END-PERFORM
           DISPLAY "----------------------------------------".

       PROCESSAR-PEDIDO.
           PERFORM CALCULAR-TOTAIS
           DISPLAY "----------------------------------------"
           DISPLAY "Total Ingredientes: " TOTAL-ING " EUR"
           DISPLAY "Valor da Pizza: " TOTAL-PIZZA " EUR"
           DISPLAY "I.V.A (23%): " TOTAL-IVA " EUR"
           DISPLAY "Total a Pagar: " TOTAL-FINAL " EUR"
           DISPLAY "Muito obrigado pela preferência!"
           DISPLAY "----------------------------------------".

       CALCULAR-TOTAIS.
           MOVE 0 TO TOTAL-ING-CALC
           PERFORM VARYING CONTADOR-ING FROM 1 BY 1
                   UNTIL CONTADOR-ING > NUM-ING
               ADD PRECO-SEL(CONTADOR-ING) TO TOTAL-ING-CALC
           END-PERFORM
           MOVE VALOR-PIZZA(TIPO-PEDIDO) TO TOTAL-PIZZA-CALC
           COMPUTE TOTAL-FINAL-CALC = TOTAL-PIZZA-CALC + TOTAL-ING-CALC
           COMPUTE TOTAL-IVA-CALC = TOTAL-FINAL-CALC * 0,23
           COMPUTE TOTAL-FINAL-CALC = TOTAL-FINAL-CALC + TOTAL-IVA-CALC
           MOVE TOTAL-ING-CALC TO TOTAL-ING
           MOVE TOTAL-PIZZA-CALC TO TOTAL-PIZZA
           MOVE TOTAL-IVA-CALC TO TOTAL-IVA
           MOVE TOTAL-FINAL-CALC TO TOTAL-FINAL.

       END PROGRAM Projeto.
