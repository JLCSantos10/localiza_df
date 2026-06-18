# localiza_df

Padronização de endereços/bairros (texto livre) para a **Região Administrativa (RA)**
do Distrito Federal, em R. Combina um **motor determinístico** (robusto e auditável)
com uma **camada opcional de Machine Learning** que **aprende com as suas correções**
e melhora a cada ciclo.

> Pensado para bases de saúde (e-SUS Notifica, SINAN, SIM) onde o campo de
> bairro vem digitado de mil formas: `"CEIL SUL"`, `"P NORTE"`, `"SAMABAIA"`,
> `"QNN 18"`, `"Vila Estrutural"`, etc.

---

## 1. O que tem na pasta

| Arquivo | Para que serve |
|---|---|
| `localiza_df.R` | **Todo o código** (motor determinístico + camada de ML). É só dar `source()`. |
| `tabela_localizacao_codeplan_2020.csv` | Tabela de localidades da CODEPLAN (auxiliar, necessária). |
| `CODQGIS_RA.xlsx` | Fonte canônica da grafia das RAs e da Região de Saúde (auxiliar, necessária). |
| `ra_treino_rotulos.csv` | *(gerado)* a "memória" do modelo: endereço → RA correta. |
| `modelo_ra.rds` | *(gerado)* o modelo de ML treinado. |

Os dois últimos são **criados automaticamente** quando você usa a camada de ML.

---

## 2. Instalação

```r
# Sempre necessários:
install.packages(c("dplyr", "stringr", "stringi", "readr", "tibble", "readxl"))

# Só para a camada de Machine Learning:
install.packages(c("tidymodels", "textrecipes", "glmnet"))
```

> Sem os pacotes de ML, **tudo continua funcionando** — só o motor determinístico
> roda. A camada de ML é um bônus que você liga quando quiser.

---

## 3. Uso rápido (padronizar uma coluna)

```r
source("localiza_df.R")            # rode a partir desta pasta

meu_df <- read.csv("meu_banco.csv")

# padroniza a coluna de endereço (troque "NM_BAIRRO" pelo nome da SUA coluna):
res <- localiza_df_ml(meu_df, col_bairro = "NM_BAIRRO")
```

`res` é o **seu data.frame + colunas novas**. As principais:

| Coluna | O que é |
|---|---|
| **`regiao_adm`** | **a RA padronizada** (é isso que você quer) |
| `regiao_adm_chave` | versão sem acento/espaço, para cruzamentos (`joins`) |
| `metodo_vinculo` | como resolveu: `match_exato`, `regra_deterministica`, `fuzzy_match`, `ml_glmnet` |
| `origem_classificacao` | `deterministica` ou `ml_glmnet` |
| `score_confianca` | confiança de 0 a 1 |
| `precisa_revisao_manual` | `TRUE` = não confiou; vale um olho humano |
| `endereco_original` / `endereco_padronizado` | texto de entrada e sua versão normalizada |

Quer também a **Região de Saúde**? Encadeie:

```r
res <- localiza_df_ml(meu_df, col_bairro = "NM_BAIRRO") |>
  vincular_regiao_saude(col_ra = "regiao_adm")
# agora há a coluna `regiao_saude`
```

> Enquanto você não treinar um modelo, `localiza_df_ml()` devolve exatamente o
> resultado determinístico (com um aviso). **Já dá para usar hoje.**

---

## 4. Como funciona (as duas camadas)

```
            ┌─────────────────────────────────────────────┐
 endereço → │ 1) MOTOR DETERMINÍSTICO  (localiza_df)       │ → resolve a maioria
            │    match exato → regra por token → fuzzy     │
            └───────────────┬─────────────────────────────┘
                            │ casos incertos (precisa_revisao_manual = TRUE)
                            ▼
            ┌─────────────────────────────────────────────┐
            │ 2) CAMADA DE ML  (se houver modelo treinado) │ → resolve o resíduo
            │    n-gramas de caractere + glmnet multinom.  │
            └─────────────────────────────────────────────┘
```

- **Camada 1 — determinística:** usa a tabela da CODEPLAN + um dicionário curado
  de apelidos do DF + comparação por similaridade (`adist`). É a mesma lógica
  consagrada do projeto, **auditável** (todo match tem método e score).
- **Camada 2 — ML:** só age nos endereços que a camada 1 marcou para revisão.
  Usa **n-gramas de caractere** (trechos de 2 a 4 letras), ótimos para lidar com
  abreviações e erros de digitação, e um classificador **glmnet** (regressão
  logística multinomial). Só aceita a predição se a confiança passar do limiar.

---

## 5. O ciclo de aprendizagem (como o ML "aprende gradualmente")

O modelo **não nasce pronto**: ele aprende com as suas correções. Cada volta do
ciclo aumenta a base de rótulos e melhora o modelo.

```
 classificar → rotular (humano corrige) → treinar → (repete) → classificar melhor
```

### Passo a passo

```r
source("localiza_df.R")

# 1) Classifica com o que houver hoje (determinístico + ML, se já treinado)
res <- localiza_df_ml(meu_df, col_bairro = "NM_BAIRRO")

# 2) Semeia rótulos com os acertos de ALTA confiança (treino "de graça")
bootstrap_rotulos_v2(res)

# 3) Exporta os casos duvidosos para revisão humana
coletar_rotulos_para_revisao(res, "ra_para_revisar.csv")
```

**4) (HUMANO)** Abra `ra_para_revisar.csv` no Excel. Há a coluna **`regiao_adm_correta`**
vazia — preencha com a RA certa nas linhas que você reconhece e salve.

```r
# 5) Incorpora as correções na base de rótulos
registrar_rotulos_ra("ra_para_revisar.csv")

# 6) (Re)treina o modelo e salva modelo_ra.rds
treinar_modelo_ra()

# 7) Confere a acurácia por validação cruzada
avaliar_modelo_ra()
```

Pronto: a próxima chamada de `localiza_df_ml()` já usa o modelo no resíduo.
**Repita os passos 1–6 sempre que quiser** que ele aprenda mais — quanto mais
correções, menos revisão manual no futuro.

### Exemplo de "antes e depois"
- `"SAMAMBA NORT"` (erro feio) → hoje cai em revisão manual.
- Você corrige no CSV → `treinar_modelo_ra()`.
- Depois, o ML reconhece pelos n-gramas e classifica como `SAMAMBAIA` sozinho
  (`metodo_vinculo = "ml_glmnet"`).

---

## 6. Funções disponíveis

| Função | Uso |
|---|---|
| `localiza_df_ml(df, col_bairro)` | **Principal.** Padroniza a coluna (determinístico + ML). |
| `localiza_df(df, col_bairro)` | Só o motor determinístico (sem ML). |
| `vincular_regiao_saude(df, col_ra)` | Adiciona a coluna `regiao_saude`. |
| `bootstrap_rotulos_v2(res)` | Cria rótulos a partir dos acertos confiáveis. |
| `coletar_rotulos_para_revisao(res, arquivo)` | Exporta o resíduo para revisão. |
| `registrar_rotulos_ra(arquivo)` | Incorpora o CSV revisado à base de rótulos. |
| `treinar_modelo_ra()` | Treina e salva o modelo. |
| `avaliar_modelo_ra()` | Acurácia por validação cruzada. |
| `prever_ra_ml(enderecos)` | Predição direta de um vetor de textos. |
| `auditoria_vinculo_regiao_administrativa(res)` | Tabela dos casos a revisar. |
| `diagnostico_vinculo_ra(res, col_verdade)` | Mede acurácia contra uma coluna-gabarito. |

---

## 7. Parâmetros úteis

`localiza_df_ml(...)`:
- `limiar_ml` (default `0.70`): confiança mínima para **aceitar** a predição do ML.
- `limiar_revisao` (default `0.85`): abaixo disso, mesmo aceitando, ainda marca
  `precisa_revisao_manual = TRUE` (vira rótulo no próximo ciclo).
- `caminho_tabela`, `caminho_qgis`, `caminho_modelo`: caminhos dos arquivos.

`treinar_modelo_ra(...)`:
- `penalty` (default `0.01`): regularização do glmnet (maior = mais simples).
- `n_gram` (default `4`): tamanho máximo do n-grama de caractere.
- `min_exemplos` (default `30`): mínimo de rótulos para treinar.

---

## 8. Observações e limitações

- Serve para **Regiões Administrativas do DF**. Para outra unidade (município,
  estado), seria preciso outra tabela de referência.
- O arquivo `tabela_localizacao_codeplan_2020.csv` tem campos pré-computados
  corrompidos (vírgulas não escapadas na descrição); por isso o código **ignora**
  essas colunas e reconstrói tudo a partir de `regiao_administrativa` + `localidade`.
- A grafia final das RAs é sempre padronizada pela fonte de verdade `CODQGIS_RA.xlsx`.
- O ML **complementa** o determinístico — ele não substitui a auditoria: todo
  resultado traz `metodo_vinculo`, `score_confianca` e `origem_classificacao`.
