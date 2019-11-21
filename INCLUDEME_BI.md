| Parameter           | Description                                                            |
| :------------------ | :--------------------------------------------------------------------- |
| **Construct X**     |                                                                        |
| gamma\_lx1          | Mean of latent true scores x (Intercept)                               |
| sigma2\_lx1         | Var of latent true scores x                                       |
| sigma2\_ux          | Var of observed scores x                                          |
| alpha\_g2           | Mean of change factor (g2)                                             |
| alpha\_g3           | Mean of change factor (g3)                                             |
| sigma2\_g2          | Var of change factor (g2)                                         |
| sigma2\_g3          | Var of change factor (g3)                                         |
| sigma\_g2lx1        | Covar of change factor (g2) with the initial true score x (lx1)   |
| sigma\_g3lx1        | Covar of change factor (g3) with the initial true score x (lx1)   |
| sigma\_g2g3         | Covar of change factors within construct x                        |
| beta\_x         | Proportional change factor x                                    |
| phi\_x              | Autoregression of change scores x                                      |
| **Construct Y**     |                                                                        |
| gamma\_ly1          | Mean of latent true scores y (Intercept)                               |
| sigma2\_ly1         | Var of latent true scores y                                       |
| sigma2\_uy          | Var of observed scores y                                          |
| alpha\_j2           | Mean of change factor (j2)                                             |
| alpha\_j3           | Mean of change factor (j3)                                             |
| sigma2\_j2          | Var of change factor (j2)                                         |
| sigma2\_j3          | Var of change factor (j3)                                         |
| sigma\_j2ly1        | Covar of change factor (j2) with the initial true score y (ly1)   |
| sigma\_j3ly1        | Covar of change factor (j3) with the initial true score y (ly1)   |
| sigma\_j2j3         | Covar of change factors within construct y                        |
| beta\_y         | Proportional change factor y                                   |
| phi\_y              | Autoregression of change scores y                                      |
| **Coupeling X & Y** |                                                                        |
| sigma\_su           | Covar of residuals x and y                                        |
| sigma\_ly1lx1       | Covar of intercepts x and y                                       |
| sigma\_g2ly1        | Covar of change factor x (g2) with the initial true score y (ly1) |
| sigma\_g3ly1        | Covar of change factor x (g3) with the initial true score y (ly1) |
| sigma\_j2lx1        | Covar of change factor y (j2) with the initial true score x (lx1) |
| sigma\_j3lx1        | Covar of change factor y (j3) with the initial true score x (lx1) |
| sigma\_j2g2         | Covar of change factors y (j2) and x (g2)                         |
| sigma\_j2g3         | Covar of change factors y (j2) and x (g3)                         |
| sigma\_j3g2         | Covar of change factors y (j3) and x (g2)                         |
| delta\_con\_xy      | Change score x (t) determined by true score y (t)                      |
| delta\_con\_yx      | Change score y (t) determined by true score x (t)                      |
| delta\_lag\_xy      | Change score x (t) determined by true score y (t-1)                    |
| delta\_lag\_yx      | Change score y (t) determined by true score x (t-1)                    |
| xi\_con\_xy         | Change score x (t) determined by change score y (t)                    |
| xi\_con\_yx         | Change score y (t) determined by change score x (t)                    |
| xi\_lag\_xy         | Change score x (t) determined by change score y (t-1)                  |
| xi\_lag\_yx         | Change score y (t) determined by change score x (t-1)                  |
