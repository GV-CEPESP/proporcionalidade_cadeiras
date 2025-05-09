# Avaliação preliminar da proporcionalidade da distribuição de cadeiras na Câmara dos Deputados

No último dia 06 de maio, a Câmara dos Deputados aprovou o PLP 177/23, com o objetivo de atender à decisão do STF na [ADO 38](https://portal.stf.jus.br/processos/detalhe.asp?incidente=5149458). Nela, o Tribunal decidiu que o Congresso Nacional estava em mora legislativa para a revisão periódica da proporcionalidade na relação deputado/população (prevista no art. 45, §1º da CF/88), e estabeleceu que o Congresso teria até 30 de junho de 2025 para sanar esta omissão. Caso ela não fosse sanada, a distribuição de cadeiras deveria ser feita pelo TSE, segundo metodologia definida quando da elaboração da Resolução-TSE n. 23.389/2013, e com base nos dados do Censo-2022.

O [PLP 177/23](https://www.camara.leg.br/proposicoesWeb/prop_mostrarintegra?codteor=2900034&filename=Tramitacao-PLP%20177/2023) propõe um aumento no número de cadeiras das atuais 513 para 531, segundo uma distribuição fixada no Anexo do PLP. Além disso, o projeto prevê futuras revisões periódicas, baseadas nos dados do Censo (iniciando-se no próximo Censo, e não com os dados do Censo de 2022, segundo o art. 4º), a partir de uma metodologia análoga à utilizada para a distribuição de assentos nas eleições proporcionais brasileiras (art. 5º), isto é, a de *maiores médias* -- mesma metodologia adotada pelo TSE na [Resolução n. 23.389/2013](https://www.migalhas.com.br/arquivos/2014/5/art20140528-01.pdf):

> *Art. 2º A distribuição das vagas terá como base os dados oficiais do censo demográfico realizado pela Fundação Instituto Brasileiro de Geografia e Estatística (IBGE), e sua utilização para o fim específico de revisão da distribuição de vagas da Câmara dos Deputados requer o cumprimento de regras próprias [...].*

> *Art. 5º Nas revisões periódicas, será calculada a quota de representação de cada unidade da Federação, conforme método de quocientes análogos ao utilizado nas eleições proporcionais, no que couber, respeitadas as representações mínimas e máximas estabelecidas na Constituição Federal.*

Em sua decisão, o STF destaca que o objetivo da revisão é corrigir eventuais distorções na proporcionalidade da relação deputado/população. Neste documento, apresentamos resultados de uma avaliação de cenários de distribuição de cadeiras e sua eventual proporcionalidade.

## Metodologia

### Metodologia para distribuição de cadeiras

Comparamos a [atual distribuição de cadeiras da Câmara dos Deputados](https://www2.camara.leg.br/a-camara/conheca/numero-de-deputados-por-estado) e aquela prevista no [anexo do PLP 177/23](https://www.camara.leg.br/proposicoesWeb/prop_mostrarintegra?codteor=2900034&filename=Tramitacao-PLP%20177/2023) com eventuais distribuições alternativas realizadas pelo método proposto no PLP 177/23 (art. 5º) e na [Resolução-TSE n. 23.389/2013](https://www.migalhas.com.br/arquivos/2014/5/art20140528-01.pdf). Todos os cálculos são apresentados no [código](/codigos/dist_cadeiras.R) deste repositório.

Inicialmente, validamos nosso algoritmo de distribuição de cadeiras pela redistribuição atual de cadeiras segundo o Censo-2010, conforme realizado pelo TSE em 2013. *Uma vez que nossos resultados finais correspondem ao declarado pelo TSE, adotamos o mesmo algoritmo para a redistribuição de cadeiras em diferentes cenários*.

### Indicador para avaliação da proporcionalidade

A dificuldade de mensuração da proporcionalidade é uma [questão notória na literatura especializada](https://doi.org/10.1093/oxfordjournals.pan.a029822). Para avaliar a proporcionalidade da distribuição de cadeiras, adaptamos o [índice de Gallagher](https://cepesp.fgv.br/saiba-mais-sobre-indicadores), também conhecido como índice dos mínimos quadrados (*least squares index*), proposto pelo cientista político Michael Gallagher em [artigo publicado na revista *Electoral Studies*](https://doi.org/10.1016/0261-3794(91)90004-c). Este índice é calculado da seguinte forma:

$$
\text{LSq} = \sqrt{ \frac{1}{2} \sum_{i=1}^{n} (c_i - p_i)^2 }
$$

onde $m_i$ é o percentual de cadeiras destinada ao estado $i$ em uma distribuição; $p_i$ é a população do estado $i$; e $n$ é o total de unidades federativas.

## Resultados

Dentre as simulações já realizadas, avaliamos (i) a desproporcionalidade geral da distribuição de Cadeiras na Câmara segundo diferentes populações de referência; (ii) o número de cadeiras a mais/menos por UF, em relação à distribuição atual, e a partir de diferentes regras de distribuição; e (iii) a desproporcionalidade da distribuição de cadeiras somente para as unidades federativas em que a magnitude (número de cadeiras) está entre 9 e 69.

### Desproporcionalidade geral

Na figura abaixo, vemos que, considerando toda a Câmara dos Deputados, se usarmos as distribuições de 513 cadeiras, seja pelo Censo-2022 ou pelo Eleitorado-2025 (TSE), teríamos uma distribuição mais proporcional do que a atual, ou aquelas produzidas com a distribuição de 531 cadeiras.

![](/relatorios/figuras/lsq_comparacao.png)

### Cadeiras adicionadas/removidas por UF

Na figura abaixo, por sua vez, observamos como uma distribuição pela população censitária de 2022, a distribuição de 531 leva a menores perdas absolutas de cadeiras por alguns estados, como Rio de Janeiro, Bahia, Rio Grande do Sul, Piauí, Paraíba e Pernambuco, quando comparados às 513 cadeiras. Além disso, em relação à distribuição proposta no PLP 177/23, alguns estados como Ceará, Goiás, Minas Gerais, Mato Grosso, Pará e Santa Catarina ganhariam mais cadeiras -- já o Rio Grande do Norte ganharia uma cadeira a menos.

![](/relatorios/figuras/diferencas_cadeiras.jpg)

### Desproporcionalidade, excetuando UFs com números mínimo e máximo de deputados

Por fim, avaliamos a desproporcionalidade considerando somente o universo de cadeiras e de UFs uma vez excluídas aquelas que têm o número mínimo (8) ou máximo (70) de cadeiras -- isto é $8 < M < 70$. Para tanto, simulamos diferentes tamanhos de Câmara dos Deputados (entre 500 e 650 cadeiras). Vemos na figura abaixo como quaisquer distribuições baseadas nos dados Censitários e a partir da metodologia de atualização proposta no próprio PLP 177/23 são mais proporcionais que a distribuição atual e aquela proposta no PLP para entrada em vigor nas eleições de 2026. Porém, vale ressaltar que entre a distribuição atual e a nova, considerando-se somente as UFs em que $8 < M < 70$, a nova distribuição é menos desproporcional.

![](/relatorios/figuras/desproporcionalidade_cadeiras.png)
