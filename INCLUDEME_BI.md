| Parameter           | Description                                                            |
| :------------------ | :--------------------------------------------------------------------- |
| **Construct X**     |                                                                        |
| gamma\_lx1          | Mean of latent true scores x (Intercept)                               |
| sigma2\_lx1         | Variance of latent true scores x                                       |
| sigma2\_ux          | Variance of observed scores x                                          |
| alpha\_g2           | Mean of change factor (g2)                                             |
| sigma2\_g2          | Variance of change factor (g2)                                         |
| sigma\_g2lx1        | Covariance of change factor (g2) with the initial true score x (lx1)   |
| beta\_x             | Proportional change factor x                                           |
| phi\_x              | Autoregression of change scores x                                      |
| **Construct Y**     |                                                                        |
| gamma\_ly1          | Mean of latent true scores y (Intercept)                               |
| sigma2\_ly1         | Variance of latent true scores y                                       |
| sigma2\_uy          | Variance of observed scores y                                          |
| alpha\_j2           | Mean of change factor (j2)                                             |
| sigma2\_j2          | Variance of change factor (j2)                                         |
| sigma\_j2ly1        | Covariance of change factor (j2) with the initial true score y (ly1)   |
| beta\_y             | Proportional change factor y                                           |
| phi\_y              | Autoregression of change scores y                                      |
| **Coupeling X & Y** |                                                                        |
| sigma\_su           | Covariance of residuals x and y                                        |
| sigma\_ly1lx1       | Covariance of intercepts x and y                                       |
| sigma\_g2ly1        | Covariance of change factor x (g2) with the initial true score y (ly1) |
| sigma\_j2lx1        | Covariance of change factor y (j2) with the initial true score x (lx1) |
| sigma\_j2g2         | Covariance of change factors y (j2) and x (g2)                         |
| delta\_con\_xy      | Change score x (t) determined by true score y (t)                      |
| delta\_con\_yx      | Change score y (t) determined by true score x (t)                      |
| delta\_lag\_xy      | Change score x (t) determined by true score y (t-1)                    |
| delta\_lag\_yx      | Change score y (t) determined by true score x (t-1)                    |
| xi\_con\_xy         | Change score x (t) determined by change score y (t)                    |
| xi\_con\_yx         | Change score y (t) determined by change score x (t)                    |
| xi\_lag\_xy         | Change score x (t) determined by change score y (t-1)                  |
| xi\_lag\_yx         | Change score y (t) determined by change score x (t-1)                  |
