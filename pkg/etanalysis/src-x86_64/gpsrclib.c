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
