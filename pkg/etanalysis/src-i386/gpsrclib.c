//Activated Region Fitting C-source.
//Wouter D. Weeda.
//Libary of functions for Gazeplot


#include<R.h>
#include<Rmath.h>
#include<R_ext/Utils.h>

//Main Shape Function (Gaussian)
void gauss(double *theta, int *np, int *dimx, int *dimy, double *gx)
{
	//Theta is vector of parameters (arranged per region)
	//np is total number of parameters used (regions*6)
	//dimx/dimy are x-y dimensions
	//gx is vector of length (dimx*dimy)=n containing model estimates

	int reg,x,y,p;
	double f,theta_x,theta_y,sig_x,sig_y,sig_xy,det_sig,dif_x,dif_y;
	double pi = 3.141593;


	//theta 1,2 = x,y coordinates
	//theta 3,4 = sd's of x,y
	//theta 5 = corr xy
	//theta 6 = amplitude

	p=0;
	for(y=1;y<(*dimy+1);y++) {
		for(x=1;x<(*dimx+1);x++) {

			f=0; //f becomes the sum of all regions  (zeroed every region)
			for(reg=0;reg<(*np);reg=reg+6) {

				//parameter coordinates
				theta_x=theta[reg+0];
				theta_y=theta[reg+1];

				//sigma matrix
				sig_x=pow(theta[reg+2],2);
				sig_xy=theta[reg+4]*theta[reg+2]*theta[reg+3];
				sig_y=pow(theta[reg+3],2);

				//determinant of sigma
				det_sig=sig_x*sig_y-sig_xy*sig_xy;
				if(det_sig < 0) det_sig=0;

				//(x-pc)
				dif_x=(x-theta_x);
				dif_y=(y-theta_y);

				//add to f gaussian value for each region
				 f=f+theta[reg+5]*(1/(2*pi*sqrt(det_sig)))*exp(-.5*((dif_x*((dif_x*(sig_y/det_sig))-(dif_y*(sig_xy/det_sig))))+(dif_y*((dif_y*(sig_x/det_sig))-(dif_x*(sig_xy/det_sig))))));

			}

		//set output vector to sum of gaussian
		gx[p]=f;
		p++;

		}
	}
}

//sums-of-squares function (including main shape function)
void ssqgauss(double *theta, double *dat, double *W, int *np, int *dimx, int *dimy, double *ss)
{

	//Theta is vector of parameters (arranged per region)
	//dat is data
	//W is vector of weights (from SSQ)
	//np is total number of parameters used (regions*6)
	//dimx/dimy are x-y dimensions
	//ss is sums-of-squares estimate

	//nreg is number of reg*p
	int reg,x,y,p;
	double f,g,theta_x,theta_y,sig_x,sig_y,sig_xy,det_sig,dif_x,dif_y;
	double pi = 3.141593;



	//theta 1,2 = x,y coordinates
	//theta 3,4 = sd's of x,y
	//theta 5 = corr xy
	//theta 6 = amplitude

	p=0;g=0;
	for(y=1;y<(*dimy+1);y++) {
		for(x=1;x<(*dimx+1);x++) {

			f=0; //f becomes the sum of all regions  (zeroed every region)
			for(reg=0;reg<(*np);reg=reg+6) {

				//parameter coordinates
				theta_x=theta[reg+0];
				theta_y=theta[reg+1];

				//sigma matrix
				sig_x=pow(theta[reg+2],2);
				sig_xy=theta[reg+4]*theta[reg+2]*theta[reg+3];
				sig_y=pow(theta[reg+3],2);

				//determinant of sigma
				det_sig=sig_x*sig_y-sig_xy*sig_xy;
				if(det_sig < 0) det_sig=0;

				//(x-pc)
				dif_x=(x-theta_x);
				dif_y=(y-theta_y);

				//add to f gaussian value for each region
				 f=f+theta[reg+5]*(1/(2*pi*sqrt(det_sig)))*exp(-.5*((dif_x*((dif_x*(sig_y/det_sig))-(dif_y*(sig_xy/det_sig))))+(dif_y*((dif_y*(sig_x/det_sig))-(dif_x*(sig_xy/det_sig))))));

			}

			//sum (data-model)^2 over voxels and weight
			g=g+pow((dat[p]-f),2)*(1/W[p]);
			p++;
		}
	}

  //set ss to g
  ss[0]=g;

}

void ssqdata(double *dat, double *W, int *n, double *ss)
{

	int i;
	double g;

	g=0e0;
	for(i=0;i<(*n);i++) {
		g=g+pow((dat[i]),2)*(1/W[i]);
	}

	ss[0]=g;
}


//(n*n) Outer product of vector length n
void outerprod(int *n, double *ivec, double *out)
{
  //n is length of vector
  //ivec is input vector
  //output is the outerproduct n*n outerproduct


  int row,col,i;
  i=0;
  for(col=0;col<(*n);col++) {
    for(row=0;row<(*n);row++) {
    	out[i]=ivec[row]*ivec[col];
    	i++;
	}
  }

}

//(n*n) Outer product of vector length n (returns in diagonal form)
void outerproddiagonal(int *n, double *ivec, double *diag)
{
  //n is length of vector
  //ivec is input vector
  //diag is vector of length n containing diagonals
  //offdiag is vector of length n^2-n/2 containing lower trial offdiagonals

  int i,j,a;
  a=0;

  //Create diagonals
  a=0;
  for(i=0;i<(*n);i++) {
    diag[a]=ivec[i]*ivec[i];
    a=a+1;
  }
}

//(n*p) first order derivative matrix
void fderiv(int *n, int *p, int *dimx, int *dimy, double *theta, double *F)
{
	//n is number of voxels
	//p is number of parameters (is regions * 6)
	//dimx/dimy are x-y dimensions
	//THETA is vector op parameters (parameters arranged per region)
	//F is vector of length (n*p)

	int x, y, i, reg, row, col;
	double Fmat[*n][*p];
	double pi = 3.141593;

	//region loop, increases by 6 for each region
	for(reg=0;reg<(*p);reg=reg+6) {
		//voxel loop, calculate derivative per voxel (x increases fastests, then y)
		i=0;
		for(y=1;y<(*dimy+1);y++) {
			for(x=1;x<(*dimx+1);x++) {
				Fmat[i][reg+0]= 0.5000000000e0 * theta[reg+5] / theta[reg+3] * (-0.1e1 * theta[reg+3] * x + theta[reg+3] * theta[reg+0] + theta[reg+4] * theta[reg+2] * y - 0.1e1 * theta[reg+4] * theta[reg+2] * theta[reg+1]) * pow(theta[reg+2], -0.2e1) / (-0.1e1 + theta[reg+4] * theta[reg+4]) * exp(0.5000000000e0 * (theta[reg+3] * theta[reg+3] * x * x - 0.2e1 * x * theta[reg+3] * theta[reg+3] * theta[reg+0] - 0.2e1 * theta[reg+3] * x * theta[reg+4] * theta[reg+2] * y + 0.2e1 * theta[reg+3] * x * theta[reg+4] * theta[reg+2] * theta[reg+1] + theta[reg+3] * theta[reg+3] * theta[reg+0] * theta[reg+0] + 0.2e1 * theta[reg+3] * theta[reg+0] * theta[reg+4] * theta[reg+2] * y - 0.2e1 * theta[reg+3] * theta[reg+0] * theta[reg+4] * theta[reg+2] * theta[reg+1] + theta[reg+2] * theta[reg+2] * y * y - 0.2e1 * y * theta[reg+2] * theta[reg+2] * theta[reg+1] + theta[reg+2] * theta[reg+2] * theta[reg+1] * theta[reg+1]) * pow(theta[reg+2], -0.2e1) * pow(theta[reg+3], -0.2e1) / (theta[reg+4] - 0.1e1) / (theta[reg+4] + 0.1e1)) * pow(-0.1e1 * theta[reg+2] * theta[reg+2] * theta[reg+3] * theta[reg+3] * (-0.1e1 + theta[reg+4] * theta[reg+4]), -0.1e1 / 0.2e1) / pi;
				Fmat[i][reg+1]= -0.1591549430e0 * theta[reg+5] / theta[reg+2] * (theta[reg+2] * y - 0.1e1 * theta[reg+2] * theta[reg+1] - 0.1e1 * theta[reg+4] * theta[reg+3] * x + theta[reg+4] * theta[reg+3] * theta[reg+0]) * pow(theta[reg+3], -0.2e1) / (-0.1e1 + theta[reg+4] * theta[reg+4]) * exp(0.5000000000e0 * (theta[reg+3] * theta[reg+3] * x * x - 0.2e1 * x * theta[reg+3] * theta[reg+3] * theta[reg+0] - 0.2e1 * theta[reg+3] * x * theta[reg+4] * theta[reg+2] * y + 0.2e1 * theta[reg+3] * x * theta[reg+4] * theta[reg+2] * theta[reg+1] + theta[reg+3] * theta[reg+3] * theta[reg+0] * theta[reg+0] + 0.2e1 * theta[reg+3] * theta[reg+0] * theta[reg+4] * theta[reg+2] * y - 0.2e1 * theta[reg+3] * theta[reg+0] * theta[reg+4] * theta[reg+2] * theta[reg+1] + theta[reg+2] * theta[reg+2] * y * y - 0.2e1 * y * theta[reg+2] * theta[reg+2] * theta[reg+1] + theta[reg+2] * theta[reg+2] * theta[reg+1] * theta[reg+1]) * pow(theta[reg+2], -0.2e1) * pow(theta[reg+3], -0.2e1) / (theta[reg+4] - 0.1e1) / (theta[reg+4] + 0.1e1)) * pow(-0.1e1 * theta[reg+2] * theta[reg+2] * theta[reg+3] * theta[reg+3] * (-0.1e1 + theta[reg+4] * theta[reg+4]), -0.1e1 / 0.2e1);
				Fmat[i][reg+2]= -0.1591549430e0 * (theta[reg+4] * theta[reg+4] * theta[reg+3] * theta[reg+2] * theta[reg+2] + theta[reg+4] * theta[reg+2] * y * theta[reg+0] - 0.1e1 * theta[reg+4] * theta[reg+2] * y * x - 0.1e1 * theta[reg+4] * theta[reg+2] * theta[reg+1] * theta[reg+0] + theta[reg+4] * theta[reg+2] * theta[reg+1] * x - 0.2e1 * theta[reg+3] * x * theta[reg+0] + theta[reg+3] * theta[reg+0] * theta[reg+0] - 0.1e1 * theta[reg+2] * theta[reg+2] * theta[reg+3] + theta[reg+3] * x * x) * exp(0.5000000000e0 * (theta[reg+3] * theta[reg+3] * x * x - 0.2e1 * x * theta[reg+3] * theta[reg+3] * theta[reg+0] - 0.2e1 * theta[reg+3] * x * theta[reg+4] * theta[reg+2] * y + 0.2e1 * theta[reg+3] * x * theta[reg+4] * theta[reg+2] * theta[reg+1] + theta[reg+3] * theta[reg+3] * theta[reg+0] * theta[reg+0] + 0.2e1 * theta[reg+3] * theta[reg+0] * theta[reg+4] * theta[reg+2] * y - 0.2e1 * theta[reg+3] * theta[reg+0] * theta[reg+4] * theta[reg+2] * theta[reg+1] + theta[reg+2] * theta[reg+2] * y * y - 0.2e1 * y * theta[reg+2] * theta[reg+2] * theta[reg+1] + theta[reg+2] * theta[reg+2] * theta[reg+1] * theta[reg+1]) * pow(theta[reg+2], -0.2e1) * pow(theta[reg+3], -0.2e1) / (theta[reg+4] - 0.1e1) / (theta[reg+4] + 0.1e1)) * theta[reg+5] * pow(theta[reg+2], -0.3e1) / theta[reg+3] / (-0.1e1 + theta[reg+4] * theta[reg+4]) * pow(-0.1e1 * theta[reg+2] * theta[reg+2] * theta[reg+3] * theta[reg+3] * (-0.1e1 + theta[reg+4] * theta[reg+4]), -0.1e1 / 0.2e1);
				Fmat[i][reg+3]= 0.1591549430e0 * exp(0.5000000000e0 * (theta[reg+3] * theta[reg+3] * x * x - 0.2e1 * x * theta[reg+3] * theta[reg+3] * theta[reg+0] - 0.2e1 * theta[reg+3] * x * theta[reg+4] * theta[reg+2] * y + 0.2e1 * theta[reg+3] * x * theta[reg+4] * theta[reg+2] * theta[reg+1] + theta[reg+3] * theta[reg+3] * theta[reg+0] * theta[reg+0] + 0.2e1 * theta[reg+3] * theta[reg+0] * theta[reg+4] * theta[reg+2] * y - 0.2e1 * theta[reg+3] * theta[reg+0] * theta[reg+4] * theta[reg+2] * theta[reg+1] + theta[reg+2] * theta[reg+2] * y * y - 0.2e1 * y * theta[reg+2] * theta[reg+2] * theta[reg+1] + theta[reg+2] * theta[reg+2] * theta[reg+1] * theta[reg+1]) * pow(theta[reg+2], -0.2e1) * pow(theta[reg+3], -0.2e1) / (theta[reg+4] - 0.1e1) / (theta[reg+4] + 0.1e1)) * theta[reg+5] * (-0.1e1 * theta[reg+4] * theta[reg+4] * theta[reg+3] * theta[reg+3] * theta[reg+2] - 0.1e1 * theta[reg+4] * theta[reg+3] * x * theta[reg+1] + theta[reg+4] * theta[reg+3] * x * y + theta[reg+4] * theta[reg+3] * theta[reg+0] * theta[reg+1] - 0.1e1 * theta[reg+4] * theta[reg+3] * theta[reg+0] * y + 0.2e1 * theta[reg+2] * y * theta[reg+1] - 0.1e1 * theta[reg+2] * theta[reg+1] * theta[reg+1] + theta[reg+2] * theta[reg+3] * theta[reg+3] - 0.1e1 * theta[reg+2] * y * y) * pow(-0.1e1 * theta[reg+2] * theta[reg+2] * theta[reg+3] * theta[reg+3] * (-0.1e1 + theta[reg+4] * theta[reg+4]), -0.1e1 / 0.2e1) / (-0.1e1 + theta[reg+4] * theta[reg+4]) / theta[reg+2] * pow(theta[reg+3], -0.3e1);
				Fmat[i][reg+4]= -0.1591549430e0 * (pow(theta[reg+4], 0.3e1) * theta[reg+3] * theta[reg+3] * theta[reg+2] * theta[reg+2] + theta[reg+3] * x * theta[reg+2] * theta[reg+1] * theta[reg+4] * theta[reg+4] - 0.1e1 * theta[reg+3] * theta[reg+0] * theta[reg+2] * theta[reg+1] * theta[reg+4] * theta[reg+4] - 0.1e1 * theta[reg+3] * x * theta[reg+2] * y * theta[reg+4] * theta[reg+4] + theta[reg+3] * theta[reg+0] * theta[reg+2] * y * theta[reg+4] * theta[reg+4] - 0.1e1 * theta[reg+4] * theta[reg+3] * theta[reg+3] * theta[reg+2] * theta[reg+2] + theta[reg+4] * theta[reg+3] * theta[reg+3] * theta[reg+0] * theta[reg+0] + theta[reg+2] * theta[reg+2] * theta[reg+4] * theta[reg+1] * theta[reg+1] - 0.2e1 * y * theta[reg+2] * theta[reg+2] * theta[reg+4] * theta[reg+1] + theta[reg+4] * theta[reg+3] * theta[reg+3] * x * x + theta[reg+2] * theta[reg+2] * theta[reg+4] * y * y - 0.2e1 * x * theta[reg+4] * theta[reg+3] * theta[reg+3] * theta[reg+0] + theta[reg+3] * x * theta[reg+2] * theta[reg+1] - 0.1e1 * theta[reg+3] * theta[reg+0] * theta[reg+2] * theta[reg+1] - 0.1e1 * theta[reg+3] * x * theta[reg+2] * y + theta[reg+3] * theta[reg+0] * theta[reg+2] * y) * theta[reg+5] * exp(0.5000000000e0 * (theta[reg+3] * theta[reg+3] * x * x - 0.2e1 * x * theta[reg+3] * theta[reg+3] * theta[reg+0] - 0.2e1 * theta[reg+3] * x * theta[reg+4] * theta[reg+2] * y + 0.2e1 * theta[reg+3] * x * theta[reg+4] * theta[reg+2] * theta[reg+1] + theta[reg+3] * theta[reg+3] * theta[reg+0] * theta[reg+0] + 0.2e1 * theta[reg+3] * theta[reg+0] * theta[reg+4] * theta[reg+2] * y - 0.2e1 * theta[reg+3] * theta[reg+0] * theta[reg+4] * theta[reg+2] * theta[reg+1] + theta[reg+2] * theta[reg+2] * y * y - 0.2e1 * y * theta[reg+2] * theta[reg+2] * theta[reg+1] + theta[reg+2] * theta[reg+2] * theta[reg+1] * theta[reg+1]) * pow(theta[reg+2], -0.2e1) * pow(theta[reg+3], -0.2e1) / (theta[reg+4] - 0.1e1) / (theta[reg+4] + 0.1e1)) * pow(-0.1e1 * theta[reg+2] * theta[reg+2] * theta[reg+3] * theta[reg+3] * (-0.1e1 + theta[reg+4] * theta[reg+4]), -0.1e1 / 0.2e1) * pow(-0.1e1 + theta[reg+4] * theta[reg+4], -0.2e1) * pow(theta[reg+2], -0.2e1) * pow(theta[reg+3], -0.2e1);
				Fmat[i][reg+5]= 0.1591549430e0 * exp(0.5000000000e0 * (theta[reg+3] * theta[reg+3] * x * x - 0.2e1 * x * theta[reg+3] * theta[reg+3] * theta[reg+0] - 0.2e1 * theta[reg+3] * x * theta[reg+4] * theta[reg+2] * y + 0.2e1 * theta[reg+3] * x * theta[reg+4] * theta[reg+2] * theta[reg+1] + theta[reg+3] * theta[reg+3] * theta[reg+0] * theta[reg+0] + 0.2e1 * theta[reg+3] * theta[reg+0] * theta[reg+4] * theta[reg+2] * y - 0.2e1 * theta[reg+3] * theta[reg+0] * theta[reg+4] * theta[reg+2] * theta[reg+1] + theta[reg+2] * theta[reg+2] * y * y - 0.2e1 * y * theta[reg+2] * theta[reg+2] * theta[reg+1] + theta[reg+2] * theta[reg+2] * theta[reg+1] * theta[reg+1]) * pow(theta[reg+2], -0.2e1) * pow(theta[reg+3], -0.2e1) / (theta[reg+4] - 0.1e1) / (theta[reg+4] + 0.1e1)) * pow(-0.1e1 * theta[reg+2] * theta[reg+2] * theta[reg+3] * theta[reg+3] * (-0.1e1 + theta[reg+4] * theta[reg+4]), -0.1e1 / 0.2e1);
				i=i+1;
			}
		}
	}


	//vectorize Fmat, and store in pointer to F
	i=0;
	for(col=0;col<(*p);col++) {
		for(row=0;row<(*n);row++) {
			F[i]=Fmat[row][col];
			i=i+1;
		}
	}

}

//(n*p) first order derivative matrix
void fderiv2(int *n, int *p, int *dimx, int *dimy, double *theta, double *F)
{
	//n is number of voxels
	//p is number of parameters (is regions * 6)
	//dimx/dimy are x-y dimensions
	//THETA is vector op parameters (parameters arranged per region)
	//F is vector of length (n*p)

	int x, y, i, reg, row, col;
	double pi = 3.141593;


	//region loop, increases by 6 for each region
	i=0;
	for(reg=0;reg<(*p);reg=reg+6) {
		//voxel loop, calculate derivative per voxel (x increases fastests, then y)
		for(y=1;y<(*dimy+1);y++) {
			for(x=1;x<(*dimx+1);x++) {
				F[i] = 0.5000000000e0 * theta[reg+5] / theta[reg+3] * (-0.1e1 * theta[reg+3] * x + theta[reg+3] * theta[reg+0] + theta[reg+4] * theta[reg+2] * y - 0.1e1 * theta[reg+4] * theta[reg+2] * theta[reg+1]) * pow(theta[reg+2], -0.2e1) / (-0.1e1 + theta[reg+4] * theta[reg+4]) * exp(0.5000000000e0 * (theta[reg+3] * theta[reg+3] * x * x - 0.2e1 * x * theta[reg+3] * theta[reg+3] * theta[reg+0] - 0.2e1 * theta[reg+3] * x * theta[reg+4] * theta[reg+2] * y + 0.2e1 * theta[reg+3] * x * theta[reg+4] * theta[reg+2] * theta[reg+1] + theta[reg+3] * theta[reg+3] * theta[reg+0] * theta[reg+0] + 0.2e1 * theta[reg+3] * theta[reg+0] * theta[reg+4] * theta[reg+2] * y - 0.2e1 * theta[reg+3] * theta[reg+0] * theta[reg+4] * theta[reg+2] * theta[reg+1] + theta[reg+2] * theta[reg+2] * y * y - 0.2e1 * y * theta[reg+2] * theta[reg+2] * theta[reg+1] + theta[reg+2] * theta[reg+2] * theta[reg+1] * theta[reg+1]) * pow(theta[reg+2], -0.2e1) * pow(theta[reg+3], -0.2e1) / (theta[reg+4] - 0.1e1) / (theta[reg+4] + 0.1e1)) * pow(-0.1e1 * theta[reg+2] * theta[reg+2] * theta[reg+3] * theta[reg+3] * (-0.1e1 + theta[reg+4] * theta[reg+4]), -0.1e1 / 0.2e1) / pi;
				i++;
			}
		}
		for(y=1;y<(*dimy+1);y++) {
			for(x=1;x<(*dimx+1);x++) {
				F[i] = -0.1591549430e0 * theta[reg+5] / theta[reg+2] * (theta[reg+2] * y - 0.1e1 * theta[reg+2] * theta[reg+1] - 0.1e1 * theta[reg+4] * theta[reg+3] * x + theta[reg+4] * theta[reg+3] * theta[reg+0]) * pow(theta[reg+3], -0.2e1) / (-0.1e1 + theta[reg+4] * theta[reg+4]) * exp(0.5000000000e0 * (theta[reg+3] * theta[reg+3] * x * x - 0.2e1 * x * theta[reg+3] * theta[reg+3] * theta[reg+0] - 0.2e1 * theta[reg+3] * x * theta[reg+4] * theta[reg+2] * y + 0.2e1 * theta[reg+3] * x * theta[reg+4] * theta[reg+2] * theta[reg+1] + theta[reg+3] * theta[reg+3] * theta[reg+0] * theta[reg+0] + 0.2e1 * theta[reg+3] * theta[reg+0] * theta[reg+4] * theta[reg+2] * y - 0.2e1 * theta[reg+3] * theta[reg+0] * theta[reg+4] * theta[reg+2] * theta[reg+1] + theta[reg+2] * theta[reg+2] * y * y - 0.2e1 * y * theta[reg+2] * theta[reg+2] * theta[reg+1] + theta[reg+2] * theta[reg+2] * theta[reg+1] * theta[reg+1]) * pow(theta[reg+2], -0.2e1) * pow(theta[reg+3], -0.2e1) / (theta[reg+4] - 0.1e1) / (theta[reg+4] + 0.1e1)) * pow(-0.1e1 * theta[reg+2] * theta[reg+2] * theta[reg+3] * theta[reg+3] * (-0.1e1 + theta[reg+4] * theta[reg+4]), -0.1e1 / 0.2e1);
				i++;
			}
		}
		for(y=1;y<(*dimy+1);y++) {
			for(x=1;x<(*dimx+1);x++) {
				F[i] = -0.1591549430e0 * (theta[reg+4] * theta[reg+4] * theta[reg+3] * theta[reg+2] * theta[reg+2] + theta[reg+4] * theta[reg+2] * y * theta[reg+0] - 0.1e1 * theta[reg+4] * theta[reg+2] * y * x - 0.1e1 * theta[reg+4] * theta[reg+2] * theta[reg+1] * theta[reg+0] + theta[reg+4] * theta[reg+2] * theta[reg+1] * x - 0.2e1 * theta[reg+3] * x * theta[reg+0] + theta[reg+3] * theta[reg+0] * theta[reg+0] - 0.1e1 * theta[reg+2] * theta[reg+2] * theta[reg+3] + theta[reg+3] * x * x) * exp(0.5000000000e0 * (theta[reg+3] * theta[reg+3] * x * x - 0.2e1 * x * theta[reg+3] * theta[reg+3] * theta[reg+0] - 0.2e1 * theta[reg+3] * x * theta[reg+4] * theta[reg+2] * y + 0.2e1 * theta[reg+3] * x * theta[reg+4] * theta[reg+2] * theta[reg+1] + theta[reg+3] * theta[reg+3] * theta[reg+0] * theta[reg+0] + 0.2e1 * theta[reg+3] * theta[reg+0] * theta[reg+4] * theta[reg+2] * y - 0.2e1 * theta[reg+3] * theta[reg+0] * theta[reg+4] * theta[reg+2] * theta[reg+1] + theta[reg+2] * theta[reg+2] * y * y - 0.2e1 * y * theta[reg+2] * theta[reg+2] * theta[reg+1] + theta[reg+2] * theta[reg+2] * theta[reg+1] * theta[reg+1]) * pow(theta[reg+2], -0.2e1) * pow(theta[reg+3], -0.2e1) / (theta[reg+4] - 0.1e1) / (theta[reg+4] + 0.1e1)) * theta[reg+5] * pow(theta[reg+2], -0.3e1) / theta[reg+3] / (-0.1e1 + theta[reg+4] * theta[reg+4]) * pow(-0.1e1 * theta[reg+2] * theta[reg+2] * theta[reg+3] * theta[reg+3] * (-0.1e1 + theta[reg+4] * theta[reg+4]), -0.1e1 / 0.2e1);
				i++;
			}
		}
		for(y=1;y<(*dimy+1);y++) {
			for(x=1;x<(*dimx+1);x++) {
				F[i] = 0.1591549430e0 * exp(0.5000000000e0 * (theta[reg+3] * theta[reg+3] * x * x - 0.2e1 * x * theta[reg+3] * theta[reg+3] * theta[reg+0] - 0.2e1 * theta[reg+3] * x * theta[reg+4] * theta[reg+2] * y + 0.2e1 * theta[reg+3] * x * theta[reg+4] * theta[reg+2] * theta[reg+1] + theta[reg+3] * theta[reg+3] * theta[reg+0] * theta[reg+0] + 0.2e1 * theta[reg+3] * theta[reg+0] * theta[reg+4] * theta[reg+2] * y - 0.2e1 * theta[reg+3] * theta[reg+0] * theta[reg+4] * theta[reg+2] * theta[reg+1] + theta[reg+2] * theta[reg+2] * y * y - 0.2e1 * y * theta[reg+2] * theta[reg+2] * theta[reg+1] + theta[reg+2] * theta[reg+2] * theta[reg+1] * theta[reg+1]) * pow(theta[reg+2], -0.2e1) * pow(theta[reg+3], -0.2e1) / (theta[reg+4] - 0.1e1) / (theta[reg+4] + 0.1e1)) * theta[reg+5] * (-0.1e1 * theta[reg+4] * theta[reg+4] * theta[reg+3] * theta[reg+3] * theta[reg+2] - 0.1e1 * theta[reg+4] * theta[reg+3] * x * theta[reg+1] + theta[reg+4] * theta[reg+3] * x * y + theta[reg+4] * theta[reg+3] * theta[reg+0] * theta[reg+1] - 0.1e1 * theta[reg+4] * theta[reg+3] * theta[reg+0] * y + 0.2e1 * theta[reg+2] * y * theta[reg+1] - 0.1e1 * theta[reg+2] * theta[reg+1] * theta[reg+1] + theta[reg+2] * theta[reg+3] * theta[reg+3] - 0.1e1 * theta[reg+2] * y * y) * pow(-0.1e1 * theta[reg+2] * theta[reg+2] * theta[reg+3] * theta[reg+3] * (-0.1e1 + theta[reg+4] * theta[reg+4]), -0.1e1 / 0.2e1) / (-0.1e1 + theta[reg+4] * theta[reg+4]) / theta[reg+2] * pow(theta[reg+3], -0.3e1);
				i++;
			}
		}
		for(y=1;y<(*dimy+1);y++) {
			for(x=1;x<(*dimx+1);x++) {
				F[i] = -0.1591549430e0 * (pow(theta[reg+4], 0.3e1) * theta[reg+3] * theta[reg+3] * theta[reg+2] * theta[reg+2] + theta[reg+3] * x * theta[reg+2] * theta[reg+1] * theta[reg+4] * theta[reg+4] - 0.1e1 * theta[reg+3] * theta[reg+0] * theta[reg+2] * theta[reg+1] * theta[reg+4] * theta[reg+4] - 0.1e1 * theta[reg+3] * x * theta[reg+2] * y * theta[reg+4] * theta[reg+4] + theta[reg+3] * theta[reg+0] * theta[reg+2] * y * theta[reg+4] * theta[reg+4] - 0.1e1 * theta[reg+4] * theta[reg+3] * theta[reg+3] * theta[reg+2] * theta[reg+2] + theta[reg+4] * theta[reg+3] * theta[reg+3] * theta[reg+0] * theta[reg+0] + theta[reg+2] * theta[reg+2] * theta[reg+4] * theta[reg+1] * theta[reg+1] - 0.2e1 * y * theta[reg+2] * theta[reg+2] * theta[reg+4] * theta[reg+1] + theta[reg+4] * theta[reg+3] * theta[reg+3] * x * x + theta[reg+2] * theta[reg+2] * theta[reg+4] * y * y - 0.2e1 * x * theta[reg+4] * theta[reg+3] * theta[reg+3] * theta[reg+0] + theta[reg+3] * x * theta[reg+2] * theta[reg+1] - 0.1e1 * theta[reg+3] * theta[reg+0] * theta[reg+2] * theta[reg+1] - 0.1e1 * theta[reg+3] * x * theta[reg+2] * y + theta[reg+3] * theta[reg+0] * theta[reg+2] * y) * theta[reg+5] * exp(0.5000000000e0 * (theta[reg+3] * theta[reg+3] * x * x - 0.2e1 * x * theta[reg+3] * theta[reg+3] * theta[reg+0] - 0.2e1 * theta[reg+3] * x * theta[reg+4] * theta[reg+2] * y + 0.2e1 * theta[reg+3] * x * theta[reg+4] * theta[reg+2] * theta[reg+1] + theta[reg+3] * theta[reg+3] * theta[reg+0] * theta[reg+0] + 0.2e1 * theta[reg+3] * theta[reg+0] * theta[reg+4] * theta[reg+2] * y - 0.2e1 * theta[reg+3] * theta[reg+0] * theta[reg+4] * theta[reg+2] * theta[reg+1] + theta[reg+2] * theta[reg+2] * y * y - 0.2e1 * y * theta[reg+2] * theta[reg+2] * theta[reg+1] + theta[reg+2] * theta[reg+2] * theta[reg+1] * theta[reg+1]) * pow(theta[reg+2], -0.2e1) * pow(theta[reg+3], -0.2e1) / (theta[reg+4] - 0.1e1) / (theta[reg+4] + 0.1e1)) * pow(-0.1e1 * theta[reg+2] * theta[reg+2] * theta[reg+3] * theta[reg+3] * (-0.1e1 + theta[reg+4] * theta[reg+4]), -0.1e1 / 0.2e1) * pow(-0.1e1 + theta[reg+4] * theta[reg+4], -0.2e1) * pow(theta[reg+2], -0.2e1) * pow(theta[reg+3], -0.2e1);
				i++;
			}
		}
		for(y=1;y<(*dimy+1);y++) {
			for(x=1;x<(*dimx+1);x++) {
				F[i] = 0.1591549430e0 * exp(0.5000000000e0 * (theta[reg+3] * theta[reg+3] * x * x - 0.2e1 * x * theta[reg+3] * theta[reg+3] * theta[reg+0] - 0.2e1 * theta[reg+3] * x * theta[reg+4] * theta[reg+2] * y + 0.2e1 * theta[reg+3] * x * theta[reg+4] * theta[reg+2] * theta[reg+1] + theta[reg+3] * theta[reg+3] * theta[reg+0] * theta[reg+0] + 0.2e1 * theta[reg+3] * theta[reg+0] * theta[reg+4] * theta[reg+2] * y - 0.2e1 * theta[reg+3] * theta[reg+0] * theta[reg+4] * theta[reg+2] * theta[reg+1] + theta[reg+2] * theta[reg+2] * y * y - 0.2e1 * y * theta[reg+2] * theta[reg+2] * theta[reg+1] + theta[reg+2] * theta[reg+2] * theta[reg+1] * theta[reg+1]) * pow(theta[reg+2], -0.2e1) * pow(theta[reg+3], -0.2e1) / (theta[reg+4] - 0.1e1) / (theta[reg+4] + 0.1e1)) * pow(-0.1e1 * theta[reg+2] * theta[reg+2] * theta[reg+3] * theta[reg+3] * (-0.1e1 + theta[reg+4] * theta[reg+4]), -0.1e1 / 0.2e1);
				i++;
			}
		}
	}

}


//(p*p) Inner sandwich part of the Sandwich Variance Estimator
void inner_sandwich(int *n, int *p, double *F, double *W, double *R, double *B)
{

	//n is number of voxels
	//p is number of parameters (is regions * 6)
	//F is vector of length (n*p) containing Jacobian
	//W is vector of length (n) containing weights from SSQ
	//Rdiag is diagonal of mean residual matrix
	//Roffdiag are offdiagonals of mean resiudal matrix (lower triangle)
	//B is vector of length p*p containing inner sandwich part

	int row, col,i;
	double *Fmat, *FW;

	Fmat = (double *) R_alloc((*n)*(*p),sizeof(double));
	FW = (double *) R_alloc((*n)*(*p),sizeof(double));


	//zerofill FW (FW is F'W matrix product)
	for(col=0;col<(*n);col++) {
		for(row=0;row<(*p);row++) {
			*(FW+(row+(col)*(*p))) = 0e0;
		}
	}


	//zerofill Bmat (Bmat is p*p matrix with inner sandwich)
	for(col=0;col<(*p);col++) {
		for(row=0;row<(*p);row++) {
			B[(row+(col)*(*p))]=0e0;
		}
	}


	//Fill Fmat matrix with F vector
	for(col=0;col<(*p);col++) {
		for(row=0;row<(*n);row++) {
			*(Fmat+(row+(col)*(*n))) = F[(row+(col)*(*n))];
		}
	}

	//Create FW=W-1RW-1 (n*n weigthed Residual matrix)
	for(col=0;col<(*n);col++) {
		for(row=0;row<(*n);row++) {
			R[(row+(col)*(*n))]=R[(row+(col)*(*n))]/(W[row]*W[col]);
		}
	}

	//Create F'(WRW) matrix (with FW part)
	for(col=0;col<(*n);col++) {
		for(row=0;row<(*p);row++) {
			for(i=0;i<(*n);i++) {
				*(FW+(row+(col)*(*p)))=*(FW+(row+(col)*(*p)))+*(Fmat+(i+(row)*(*n)))*R[(i+(col)*(*n))];
			}
		}
	}


	//Create B=(F'WRW)F matrix
	for(col=0;col<(*p);col++) {
		for(row=0;row<(*p);row++) {
			for(i=0;i<(*n);i++) {
				B[(row+(col)*(*p))]=B[(row+(col)*(*p))]+*(FW+(row+(i)*(*p)))**(Fmat+(i+(col)*(*n)));
			}
		}
	}

}

