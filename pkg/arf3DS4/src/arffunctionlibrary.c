//Activated Region Fitting. Version 2 Build 1.
//Copyright (c) 2009 Wouter Weeda.
//Libary of functions for ARF


#include<R.h>
#include<math.h>
#include<stdio.h>


void gauss(double *theta, int *np, int *dimx, int *dimy, int *dimz, double *gx)
{

	int reg,x,y,z,p;
	double f,theta_x,theta_y,theta_z,sig_x,sig_y,sig_z,sig_xy,sig_xz,sig_yz,det_sig,dif_x,dif_y,dif_z;

	//theta 1,2,3 = x,y,z coordinates
	//theta 4,5,6 = sd's of x,y,z
	//theta 7,8,9 = corr xy, xz, yz
	//theta 10    = amplitude

	p=0;
	for(z=1;z<(*dimz+1);z++) {
		for(y=1;y<(*dimy+1);y++) {
			for(x=1;x<(*dimx+1);x++) {

				f=0; //f becomes the sum of all regions  (zeroed every region)

				for(reg=0;reg<(*np);reg=reg+10) {
					//parameter coordinates
					theta_x=theta[reg+0];
					theta_y=theta[reg+1];
					theta_z=theta[reg+2];

					//sigma matrix
					sig_x=pow(theta[reg+3],2);
					sig_xy=theta[reg+6]*theta[reg+4]*theta[reg+3];
					sig_xz=theta[reg+7]*theta[reg+5]*theta[reg+3];
					sig_y=pow(theta[reg+4],2);
					sig_yz=theta[reg+8]*theta[reg+4]*theta[reg+5];
					sig_z=pow(theta[reg+5],2);


					//determinant of sigma
					det_sig=sig_x*sig_y*sig_z-sig_x*sig_yz*sig_yz-sig_xy*sig_xy*sig_z+sig_xy*sig_xz*sig_yz+sig_xz*sig_xy*sig_yz-sig_xz*sig_y*sig_xz;
					if(det_sig < 0) det_sig=0;

					//(x-pc)
					dif_x=(x-theta_x);
					dif_y=(y-theta_y);
					dif_z=(z-theta_z);

					//add to f gaussian value for each region
					f=f+theta[reg+9]*(1/(pow(sqrt(2*M_PI),3)*sqrt(det_sig)))*exp(-.5*(dif_x*(dif_x*(sig_y*sig_z-sig_yz*sig_yz)+dif_y*(sig_yz*sig_xz-sig_xy*sig_z)+dif_z*(sig_xy*sig_yz-sig_y*sig_xz))/det_sig+dif_y*(dif_x*(sig_xz*sig_yz-sig_z*sig_xy)+dif_y*(sig_x*sig_z-sig_xz*sig_xz)+dif_z*(sig_xy*sig_xz-sig_x*sig_yz))/det_sig+dif_z*(dif_x*(sig_xy*sig_yz-sig_xz*sig_y)+dif_y*(sig_xz*sig_xy-sig_yz*sig_x)+dif_z*(sig_x*sig_y-sig_xy*sig_xy))/det_sig));

				}

				gx[p]=f; //set output vector to sum of gaussian
				p++;

			}
		}
	}
}

void ssqgauss(double *theta, double *dat, double *W, int *np, int *dimx, int *dimy, int *dimz, double *ss)
{

	int reg,x,y,z,p;
	double f,g,theta_x,theta_y,theta_z,sig_x,sig_y,sig_z,sig_xy,sig_xz,sig_yz,det_sig,dif_x,dif_y,dif_z;

	//theta 1,2,3 = x,y,z coordinates
	//theta 4,5,6 = sd's of x,y,z
	//theta 7,8,9 = corr xy, xz, yz
	//theta 10    = amplitude

	p=0;
	g=0e0;
	for(z=1;z<(*dimz+1);z++) {
		for(y=1;y<(*dimy+1);y++) {
			for(x=1;x<(*dimx+1);x++) {

				f=0; //f becomes the sum of all regions  (zeroed every region)

				for(reg=0;reg<(*np);reg=reg+10) {
					//parameter coordinates
					theta_x=theta[reg+0];
					theta_y=theta[reg+1];
					theta_z=theta[reg+2];

					//sigma matrix
					sig_x=pow(theta[reg+3],2);
					sig_xy=theta[reg+6]*theta[reg+4]*theta[reg+3];
					sig_xz=theta[reg+7]*theta[reg+5]*theta[reg+3];
					sig_y=pow(theta[reg+4],2);
					sig_yz=theta[reg+8]*theta[reg+4]*theta[reg+5];
					sig_z=pow(theta[reg+5],2);


					//determinant of sigma
					det_sig=sig_x*sig_y*sig_z-sig_x*sig_yz*sig_yz-sig_xy*sig_xy*sig_z+sig_xy*sig_xz*sig_yz+sig_xz*sig_xy*sig_yz-sig_xz*sig_y*sig_xz;
					if(det_sig < 0) det_sig=0;

					//(x-pc)
					dif_x=(x-theta_x);
					dif_y=(y-theta_y);
					dif_z=(z-theta_z);

					//add to f gaussian value for each region
					f=f+theta[reg+9]*(1/(pow(sqrt(2*M_PI),3)*sqrt(det_sig)))*exp(-.5*(dif_x*(dif_x*(sig_y*sig_z-sig_yz*sig_yz)+dif_y*(sig_yz*sig_xz-sig_xy*sig_z)+dif_z*(sig_xy*sig_yz-sig_y*sig_xz))/det_sig+dif_y*(dif_x*(sig_xz*sig_yz-sig_z*sig_xy)+dif_y*(sig_x*sig_z-sig_xz*sig_xz)+dif_z*(sig_xy*sig_xz-sig_x*sig_yz))/det_sig+dif_z*(dif_x*(sig_xy*sig_yz-sig_xz*sig_y)+dif_y*(sig_xz*sig_xy-sig_yz*sig_x)+dif_z*(sig_x*sig_y-sig_xy*sig_xy))/det_sig));

				}

				//sum (data-model)^2 over voxels and weight
				g=g+pow((dat[p]-f),2)*(1/W[p]);
				p++;

			}
		}
	}

	//set ss to g
	ss[0]=g;
}


void innerSW(int *n, int *p, int *trials, char **fnderiv, char **fnresid, char **fnweight, double *B)
{

	int i,j,k,l,m, Brow, Bcol;
	FILE *fderiv, *fresid, *fweight;
	double *Fv,*Ft, *Rv, *Wv, *Mr, s, Bm[*p][*p];

	Fv = (double *) R_alloc(*n,sizeof(double));
	Ft = (double *) R_alloc(*n,sizeof(double));
	Rv = (double *) R_alloc(*n,sizeof(double));
 	Wv = (double *) R_alloc(*n,sizeof(double));
 	Mr = (double *) R_alloc(*n,sizeof(double));

 	fderiv=fopen(*fnderiv,"r"); //NxP derivs vector (n incr. fastest)
 	fresid=fopen(*fnresid,"r"); //Nxtrial residual vector (n incr fastest)
 	fweight=fopen(*fnweight,"r"); //N vector of weights

 	fread(Wv,sizeof(double),*n,fweight);
 	fclose(fweight);

 	for(m=0;m<(*n);m++) {
 		*(Mr+m)=0e0;
 	}

 	// OFF DIAGONAL B, LOOP
	for(Brow=0;Brow<(*p-1);Brow++) { //BVEC ROW LOOP

		fseek(fderiv,sizeof(double)*(Brow**n),SEEK_SET);
		fread(Fv,sizeof(double),*n,fderiv);

		for(j=0;j<(*n);j++) { // R matrix column loop


			for(l=0;l<(*trials);l++) { //trial loop

				fseek(fresid,sizeof(double)*((l)**n),SEEK_SET);
				fread(Rv,sizeof(double),*n,fresid);

				for(m=0;m<(*n);m++) {
					*(Mr+m)=*(Mr+m)+((1/pow((double) *trials,2))**(Rv+j)**(Rv+m));
				}

			}

			s=0;
			for(i=0;i<(*n);i++) {  // R matrix row loop
				s=s+*(Fv+i)*(*(Mr+i)/(*(Wv+j)**(Wv+i)));
				*(Mr+i)=0e0;

			}
			*(Ft+j)=s;

		}

		for(Bcol=(Brow+1);Bcol<(*p);Bcol++) { //BVEC COL LOOP
			fseek(fderiv,sizeof(double)*(Bcol**n),SEEK_SET);
			fread(Fv,sizeof(double),*n,fderiv);

			s=0;
			for(i=0;i<(*n);i++) { // (t(F)%*%R)%*%F LOOP
				s=s+Fv[i]*Ft[i];

			}

			Bm[Brow][Bcol]=s;
			Bm[Bcol][Brow]=s;
			//Rprintf("Done %d %d\n",Brow,Bcol);

		}

	}

	for(m=0;m<(*n);m++) {
		*(Mr+m)=0e0;
	}

	//DIAGONAL B, LOOP
	for(Brow=0;Brow<(*p);Brow++) { //BVEC ROW LOOP

		fseek(fderiv,sizeof(double)*(Brow**n),SEEK_SET);
		fread(Fv,sizeof(double),*n,fderiv);

		for(j=0;j<(*n);j++) { // R matrix column loop

			for(l=0;l<(*trials);l++) { //trial loop

				fseek(fresid,sizeof(double)*((l)**n),SEEK_SET);
				fread(Rv,sizeof(double),*n,fresid);

				for(m=0;m<(*n);m++) {
					*(Mr+m)=*(Mr+m)+((1/pow((double) *trials,2))**(Rv+j)**(Rv+m));
				}
			}

			s=0;
			for(i=0;i<(*n);i++) {  // R matrix row loop
				s=s+*(Fv+i)*(*(Mr+i)/(*(Wv+j)**(Wv+i)));
				*(Mr+i)=0e0;
			}

			*(Ft+j)=s;

		}

		s=0;
		for(i=0;i<(*n);i++) { // (t(F)%*%R)%*%F LOOP
			s=s+Fv[i]*Ft[i];
		}

		Bm[Brow][Brow]=s;
		//Rprintf("Done %d %d\n",Brow,Brow);
	}



	k=0;
	for(Bcol=0;Bcol<(*p);Bcol++) {
		for(Brow=0;Brow<(*p);Brow++) {
			*(B+k)=Bm[Brow][Bcol];
			k++;
		}
	}

	fclose(fderiv);
	fclose(fresid);

}


void innerSWdiag(int *n, int *p, int *trials, char **fnderiv, char **fnresid, char **fnweight, double *B)
{

	int i,j,k,l,m, Brow, Bcol;
	FILE *fderiv, *fresid, *fweight;
	double *Fv,*Ft, *Rv, *Wv, *Mr, s, Bm[*p][*p];

	Fv = (double *) R_alloc(*n,sizeof(double));
	Ft = (double *) R_alloc(*n,sizeof(double));
	Rv = (double *) R_alloc(*n,sizeof(double));
 	Wv = (double *) R_alloc(*n,sizeof(double));
 	Mr = (double *) R_alloc(*n,sizeof(double));

 	fderiv=fopen(*fnderiv,"r"); //NxP derivs vector (n incr. fastest)
 	fresid=fopen(*fnresid,"r"); //Nxtrial residual vector (n incr fastest)
 	fweight=fopen(*fnweight,"r"); //N vector of weights

 	fread(Wv,sizeof(double),*n,fweight);
 	fclose(fweight);

 	for(m=0;m<(*n);m++) {
 		*(Mr+m)=0e0;
 	}

	for(l=0;l<(*trials);l++) { //make mean residual

		fseek(fresid,sizeof(double)*((l)**n),SEEK_SET);
		fread(Rv,sizeof(double),*n,fresid);

		for(m=0;m<(*n);m++) {
			*(Mr+m)=*(Mr+m)+((1/pow((double) *trials,2))**(Rv+m)**(Rv+m));
		}
	}

	for(m=0;m<(*n);m++) { //weight mean residuals
		*(Mr+m)=*(Mr+m)/(*(Wv+m)**(Wv+m));
	}


	fclose(fresid);

 	// OFF DIAGONAL B, LOOP
	for(Brow=0;Brow<(*p-1);Brow++) { //BVEC ROW LOOP

		fseek(fderiv,sizeof(double)*(Brow**n),SEEK_SET);
		fread(Fv,sizeof(double),*n,fderiv);

		for(j=0;j<(*n);j++) { // R matrix column loop

			*(Ft+j)=*(Fv+j)**(Mr+j);

		}


		for(Bcol=(Brow+1);Bcol<(*p);Bcol++) { //BVEC COL LOOP
			fseek(fderiv,sizeof(double)*(Bcol**n),SEEK_SET);
			fread(Fv,sizeof(double),*n,fderiv);

			s=0;
			for(i=0;i<(*n);i++) { // (t(F)%*%R)%*%F LOOP
				s=s+Fv[i]*Ft[i];

			}

			Bm[Brow][Bcol]=s;
			Bm[Bcol][Brow]=s;

		}
	}

	//DIAGONAL B, LOOP
	for(Brow=0;Brow<(*p);Brow++) { //BVEC ROW LOOP

		fseek(fderiv,sizeof(double)*(Brow**n),SEEK_SET);
		fread(Fv,sizeof(double),*n,fderiv);

		for(j=0;j<(*n);j++) { // R matrix column loop

			*(Ft+j)=*(Fv+j)**(Mr+j);

		}

		s=0;
		for(i=0;i<(*n);i++) { // (t(F)%*%R)%*%F LOOP
			s=s+Fv[i]*Ft[i];

		}

		Bm[Brow][Brow]=s;

	}


	k=0;
	for(Bcol=0;Bcol<(*p);Bcol++) {
		for(Brow=0;Brow<(*p);Brow++) {
			*(B+k)=Bm[Brow][Bcol];
			k++;
		}
	}

	fclose(fderiv);

}

void dfgaussFile(int *np, int *dimx, int *dimy, int *dimz, double *thetavec, char **filename)
{

	void dftheta0();
	void dftheta1();
	void dftheta2();
	void dftheta3();
	void dftheta4();
	void dftheta5();
	void dftheta6();
	void dftheta7();
	void dftheta8();
	void dftheta9();



	int i,n=(*dimx)*(*dimy)*(*dimz), reg;
	double *grad, *theta;
	grad = (double *) R_alloc((n),sizeof(double));
	theta = (double *) R_alloc((10),sizeof(double));


	FILE *f;
	f=fopen(*filename,"w");

	for(reg=0;reg<(*np);reg=reg+10) {

		for(i=0;i<10;i++) {
			*(theta+i)=*(thetavec+reg+i);
		}

		dftheta0(theta,dimx,dimy,dimz,grad);
		fwrite(grad,sizeof(double),n,f);
		dftheta1(theta,dimx,dimy,dimz,grad);
		fwrite(grad,sizeof(double),n,f);
		dftheta2(theta,dimx,dimy,dimz,grad);
		fwrite(grad,sizeof(double),n,f);
		dftheta3(theta,dimx,dimy,dimz,grad);
		fwrite(grad,sizeof(double),n,f);
		dftheta4(theta,dimx,dimy,dimz,grad);
		fwrite(grad,sizeof(double),n,f);
		dftheta5(theta,dimx,dimy,dimz,grad);
		fwrite(grad,sizeof(double),n,f);
		dftheta6(theta,dimx,dimy,dimz,grad);
		fwrite(grad,sizeof(double),n,f);
		dftheta7(theta,dimx,dimy,dimz,grad);
		fwrite(grad,sizeof(double),n,f);
		dftheta8(theta,dimx,dimy,dimz,grad);
		fwrite(grad,sizeof(double),n,f);
		dftheta9(theta,dimx,dimy,dimz,grad);
		fwrite(grad,sizeof(double),n,f);

	}

	fclose(f);
}

void dftheta0(double *theta, int *dimx, int *dimy, int *dimz, double *grad) {

    double ev[199];
    int x,y,z,p;

    p=0;
	for(z=1;z<(*dimz+1);z++) {
		for(y=1;y<(*dimy+1);y++) {
			for(x=1;x<(*dimx+1);x++) {
				ev[4] = pow(*(theta+3),2);
				ev[5] = pow(*(theta+4),2);
				ev[6] = ev[4] * ev[5];
				ev[7] = pow(*(theta+5),2);
				ev[10] = *(theta+8) * *(theta+4) * *(theta+5);
				ev[11] = ev[4] * ev[10];
				ev[15] = *(theta+6) * *(theta+4) * *(theta+3);
				ev[16] = ev[15] * ev[15];
				ev[20] = *(theta+7) * *(theta+5) * *(theta+3);
				ev[21] = ev[15] * ev[20];
				ev[24] = ev[20] * ev[15];
				ev[27] = ev[20] * ev[5];
				ev[29] = ev[6] * ev[7] - ev[11] * ev[10] - ev[16] * ev[7] + ev[21] * ev[10] + ev[24] * ev[10] - ev[27] * ev[20];
				ev[33] = *(theta+9) * (1/(pow(sqrt(2 * M_PI),3) * sqrt(ev[29])));
				ev[35] = x - *(theta+0);
				ev[39] = ev[35] * (ev[5] * ev[7] - ev[10] * ev[10]);
				ev[40] = y - *(theta+1);
				ev[46] = z - *(theta+2);
				ev[47] = ev[15] * ev[10];
				ev[51] = ev[39] + ev[40] * (ev[10] * ev[20] - ev[15] * ev[7]) + ev[46] * (ev[47] - ev[5] * ev[20]);
				ev[56] = ev[20] * ev[10] - ev[7] * ev[15];
				ev[69] = ev[47] - ev[27];
				ev[82] = exp(-0.5 * (ev[35] * ev[51]/ev[29] + ev[40] * (ev[35] * ev[56] + ev[40] * (ev[4] * ev[7] - ev[20] * ev[20]) + ev[46] * (ev[21] - ev[11]))/ev[29] + ev[46] * (ev[35] * ev[69] + ev[40] * (ev[24] - ev[10] * ev[4]) + ev[46] * (ev[6] - ev[16]))/ev[29]));

				*(grad+p) = ev[33] * (ev[82] * (0.5 * (ev[46] * ev[69]/ev[29] + (ev[40] * ev[56]/ev[29] + (ev[39] + ev[51])/ev[29]))));

				p++;
			}
		}
	}

}

void dftheta1(double *theta, int *dimx, int *dimy, int *dimz, double *grad) {

    double ev[199];
    int x,y,z,p;

    p=0;
	for(z=1;z<(*dimz+1);z++) {
		for(y=1;y<(*dimy+1);y++) {
			for(x=1;x<(*dimx+1);x++) {
				ev[4] = pow(*(theta+3),2);
				ev[5] = pow(*(theta+4),2);
				ev[6] = ev[4] * ev[5];
				ev[7] = pow(*(theta+5),2);
				ev[10] = *(theta+8) * *(theta+4) * *(theta+5);
				ev[11] = ev[4] * ev[10];
				ev[15] = *(theta+6) * *(theta+4) * *(theta+3);
				ev[16] = ev[15] * ev[15];
				ev[20] = *(theta+7) * *(theta+5) * *(theta+3);
				ev[21] = ev[15] * ev[20];
				ev[24] = ev[20] * ev[15];
				ev[27] = ev[20] * ev[5];
				ev[29] = ev[6] * ev[7] - ev[11] * ev[10] - ev[16] * ev[7] + ev[21] * ev[10] + ev[24] * ev[10] - ev[27] * ev[20];
				ev[33] = *(theta+9) * (1/(pow(sqrt(2 * M_PI),3) * sqrt(ev[29])));
				ev[35] = x - *(theta+0);
				ev[40] = y - *(theta+1);
				ev[43] = ev[10] * ev[20] - ev[15] * ev[7];
				ev[46] = z - *(theta+2);
				ev[47] = ev[15] * ev[10];
				ev[61] = ev[40] * (ev[4] * ev[7] - ev[20] * ev[20]);
				ev[65] = ev[35] * (ev[20] * ev[10] - ev[7] * ev[15]) + ev[61] + ev[46] * (ev[21] - ev[11]);
				ev[72] = ev[24] - ev[10] * ev[4];
				ev[82] = exp(-0.5 * (ev[35] * (ev[35] * (ev[5] * ev[7] - ev[10] * ev[10]) + ev[40] * ev[43] + ev[46] * (ev[47] - ev[5] * ev[20]))/ev[29] + ev[40] * ev[65]/ev[29] + ev[46] * (ev[35] * (ev[47] - ev[27]) + ev[40] * ev[72] + ev[46] * (ev[6] - ev[16]))/ev[29]));

				*(grad+p) = ev[33] * (ev[82] * (0.5 * (ev[46] * ev[72]/ev[29] + ((ev[61] + ev[65])/ev[29] + ev[35] * ev[43]/ev[29]))));

				p++;
			}
		}
	}
}

void dftheta2(double *theta, int *dimx, int *dimy, int *dimz, double *grad) {

    double ev[199];
    int x,y,z,p;

    p=0;
	for(z=1;z<(*dimz+1);z++) {
		for(y=1;y<(*dimy+1);y++) {
			for(x=1;x<(*dimx+1);x++) {
     			ev[4] = pow(*(theta+3),2);
				ev[5] = pow(*(theta+4),2);
				ev[6] = ev[4] * ev[5];
				ev[7] = pow(*(theta+5),2);
				ev[10] = *(theta+8) * *(theta+4) * *(theta+5);
				ev[11] = ev[4] * ev[10];
				ev[15] = *(theta+6) * *(theta+4) * *(theta+3);
				ev[16] = ev[15] * ev[15];
				ev[20] = *(theta+7) * *(theta+5) * *(theta+3);
				ev[21] = ev[15] * ev[20];
				ev[24] = ev[20] * ev[15];
				ev[27] = ev[20] * ev[5];
				ev[29] = ev[6] * ev[7] - ev[11] * ev[10] - ev[16] * ev[7] + ev[21] * ev[10] + ev[24] * ev[10] - ev[27] * ev[20];
				ev[33] = *(theta+9) * (1/(pow(sqrt(2 * M_PI),3) * sqrt(ev[29])));
				ev[35] = x - *(theta+0);
				ev[40] = y - *(theta+1);
				ev[46] = z - *(theta+2);
				ev[47] = ev[15] * ev[10];
				ev[49] = ev[47] - ev[5] * ev[20];
				ev[63] = ev[21] - ev[11];
				ev[76] = ev[46] * (ev[6] - ev[16]);
				ev[77] = ev[35] * (ev[47] - ev[27]) + ev[40] * (ev[24] - ev[10] * ev[4]) + ev[76];
				ev[82] = exp(-0.5 * (ev[35] * (ev[35] * (ev[5] * ev[7] - ev[10] * ev[10]) + ev[40] * (ev[10] * ev[20] - ev[15] * ev[7]) +  ev[46] * ev[49])/ev[29] + ev[40] * (ev[35] * (ev[20] * ev[10] - ev[7] * ev[15]) + ev[40] * (ev[4] * ev[7] - ev[20] * ev[20]) + ev[46] * ev[63])/ev[29] + ev[46] * ev[77]/ev[29]));

				*(grad+p) = ev[33] * (ev[82] * (0.5 * ((ev[76] + ev[77])/ev[29] + (ev[40] * ev[63]/ev[29] + ev[35] * ev[49]/ev[29]))));

				p++;
			}
		}
	}
}


void dftheta3(double *theta, int *dimx, int *dimy, int *dimz, double *grad) {

    double ev[199];
    int x,y,z,p;

    p=0;
	for(z=1;z<(*dimz+1);z++) {
		for(y=1;y<(*dimy+1);y++) {
			for(x=1;x<(*dimx+1);x++) {
				ev[3] = pow(sqrt(2 * M_PI),3);
				ev[4] = pow(*(theta+3),2);
				ev[5] = pow(*(theta+4),2);
				ev[6] = ev[4] * ev[5];
				ev[7] = pow(*(theta+5),2);
				ev[10] = *(theta+8) * *(theta+4) * *(theta+5);
				ev[11] = ev[4] * ev[10];
				ev[14] = *(theta+6) * *(theta+4);
				ev[15] = ev[14] * *(theta+3);
				ev[16] = ev[15] * ev[15];
				ev[19] = *(theta+7) * *(theta+5);
				ev[20] = ev[19] * *(theta+3);
				ev[21] = ev[15] * ev[20];
				ev[24] = ev[20] * ev[15];
				ev[27] = ev[20] * ev[5];
				ev[29] = ev[6] * ev[7] - ev[11] * ev[10] - ev[16] * ev[7] + ev[21] * ev[10] + ev[24] * ev[10] - ev[27] * ev[20];
				ev[31] = ev[3] * sqrt(ev[29]);
				ev[33] = *(theta+9) * (1/ev[31]);
				ev[35] = x - *(theta+0);
				ev[40] = y - *(theta+1);
				ev[46] = z - *(theta+2);
				ev[47] = ev[15] * ev[10];
				ev[52] = ev[35] * (ev[35] * (ev[5] * ev[7] - ev[10] * ev[10]) + ev[40] * (ev[10] * ev[20] - ev[15] * ev[7]) + ev[46] * (ev[47] - ev[5] * ev[20]));
				ev[66] = ev[40] * (ev[35] * (ev[20] * ev[10] - ev[7] * ev[15]) + ev[40] * (ev[4] * ev[7] - ev[20] * ev[20]) + ev[46] * (ev[21] - ev[11]));
				ev[78] = ev[46] * (ev[35] * (ev[47] - ev[27]) + ev[40] * (ev[24] - ev[10] * ev[4]) + ev[46] * (ev[6] - ev[16]));
				ev[82] = exp(-0.5 * (ev[52]/ev[29] + ev[66]/ev[29] + ev[78]/ev[29]));
				ev[88] = ev[14] * ev[10];
				ev[95] = 2 * *(theta+3);
				ev[96] = ev[95] * ev[5];
				ev[98] = ev[95] * ev[10];
				ev[103] = ev[14] * ev[15] + ev[15] * ev[14];
				ev[108] = ev[14] * ev[20] + ev[15] * ev[19];
				ev[113] = ev[19] * ev[15] + ev[20] * ev[14];
				ev[116] = ev[19] * ev[5];
				ev[120] = ev[96] * ev[7] - ev[98] * ev[10] - ev[103] * ev[7] + ev[108] * ev[10] + ev[113] * ev[10] - (ev[116] * ev[20] + ev[27] * ev[19]);
				ev[122] = pow(ev[29],2);

				*(grad+p)= -(ev[33] * (ev[82] * (0.5 * (ev[35] * (ev[40] * (ev[10] * ev[19] - ev[14] * ev[7]) + ev[46] * (ev[88] - ev[5] * ev[19]))/ev[29] - ev[52] * ev[120]/ev[122] + (ev[40] * (ev[35] * (ev[19] * ev[10] - ev[7] * ev[14]) + ev[40] * (ev[95] * ev[7] - (ev[19] * ev[20] + ev[20] * ev[19])) + ev[46] * (ev[108] - ev[98]))/ev[29] - ev[66] * ev[120]/ev[122]) + (ev[46] * (ev[35] * (ev[88] - ev[116]) + ev[40] * (ev[113] - ev[10] * ev[95]) + ev[46] * (ev[96] - ev[103]))/ev[29] - ev[78] * ev[120]/ev[122])))) + *(theta+9) * (ev[3] * (0.5 * (ev[120] * (1/sqrt(ev[29]))))/pow(ev[31],2)) * ev[82]);

				p++;
			}
		}
	}
}

void dftheta4(double *theta, int *dimx, int *dimy, int *dimz, double *grad) {

    double ev[199];
    int x,y,z,p;

    p=0;
	for(z=1;z<(*dimz+1);z++) {
		for(y=1;y<(*dimy+1);y++) {
			for(x=1;x<(*dimx+1);x++) {
				ev[3] = pow(sqrt(2 * M_PI),3);
				ev[4] = pow(*(theta+3),2);
				ev[5] = pow(*(theta+4),2);
				ev[6] = ev[4] * ev[5];
				ev[7] = pow(*(theta+5),2);
				ev[10] = *(theta+8) * *(theta+4) * *(theta+5);
				ev[11] = ev[4] * ev[10];
				ev[15] = *(theta+6) * *(theta+4) * *(theta+3);
				ev[16] = ev[15] * ev[15];
				ev[20] = *(theta+7) * *(theta+5) * *(theta+3);
				ev[21] = ev[15] * ev[20];
				ev[24] = ev[20] * ev[15];
				ev[27] = ev[20] * ev[5];
				ev[29] = ev[6] * ev[7] - ev[11] * ev[10] - ev[16] * ev[7] + ev[21] * ev[10] + ev[24] * ev[10] - ev[27] * ev[20];
				ev[31] = ev[3] * sqrt(ev[29]);
				ev[33] = *(theta+9) * (1/ev[31]);
				ev[35] = x - *(theta+0);
				ev[40] = y - *(theta+1);
				ev[46] = z - *(theta+2);
				ev[47] = ev[15] * ev[10];
				ev[52] = ev[35] * (ev[35] * (ev[5] * ev[7] - ev[10] * ev[10]) + ev[40] * (ev[10] * ev[20] - ev[15] * ev[7]) + ev[46] * (ev[47] - ev[5] * ev[20]));
				ev[66] = ev[40] * (ev[35] * (ev[20] * ev[10] - ev[7] * ev[15]) + ev[40] * (ev[4] * ev[7] - ev[20] * ev[20]) + ev[46] * (ev[21] - ev[11]));
				ev[78] = ev[46] * (ev[35] * (ev[47] - ev[27]) + ev[40] * (ev[24] - ev[10] * ev[4]) + ev[46] * (ev[6] - ev[16]));
				ev[82] = exp(-0.5 * (ev[52]/ev[29] + ev[66]/ev[29] + ev[78]/ev[29]));
				ev[84] = 2 * *(theta+4);
				ev[86] = *(theta+8) * *(theta+5);
				ev[93] = *(theta+6) * *(theta+3);
				ev[100] = ev[93] * ev[10] + ev[15] * ev[86];
				ev[107] = ev[4] * ev[84];
				ev[109] = ev[4] * ev[86];
				ev[116] = ev[93] * ev[15] + ev[15] * ev[93];
				ev[119] = ev[93] * ev[20];
				ev[124] = ev[20] * ev[93];
				ev[129] = ev[20] * ev[84];
				ev[131] = ev[107] * ev[7] - (ev[109] * ev[10] + ev[11] * ev[86]) - ev[116] * ev[7] + (ev[119] * ev[10] + ev[21] * ev[86]) + (ev[124] * ev[10] + ev[24] * ev[86]) - ev[129] * ev[20];
				ev[133] = pow(ev[29],2);

				*(grad+p) = -(ev[33] * (ev[82] * (0.5 * (ev[35] * (ev[35] * (ev[84] * ev[7] - (ev[86] * ev[10] + ev[10] * ev[86])) + ev[40] * (ev[86] * ev[20] - ev[93] * ev[7]) + ev[46] * (ev[100] - ev[84] * ev[20]))/ev[29] - ev[52] * ev[131]/ev[133] + (ev[40] * (ev[35] * (ev[20] * ev[86] - ev[7] * ev[93]) + ev[46] * (ev[119] - ev[109]))/ev[29] - ev[66] * ev[131]/ev[133]) + (ev[46] * (ev[35] * (ev[100] - ev[129]) + ev[40] * (ev[124] - ev[86] * ev[4]) + ev[46] * (ev[107] - ev[116]))/ev[29] - ev[78] * ev[131]/ev[133])))) + *(theta+9) * (ev[3] * (0.5 * (ev[131] * (1/sqrt(ev[29]))))/pow(ev[31],2)) * ev[82]);

				p++;
			}
		}
	}
}

void dftheta5(double *theta, int *dimx, int *dimy, int *dimz, double *grad) {

    double ev[199];
    int x,y,z,p;

    p=0;
	for(z=1;z<(*dimz+1);z++) {
		for(y=1;y<(*dimy+1);y++) {
			for(x=1;x<(*dimx+1);x++) {

				ev[3] = pow(sqrt(2 * M_PI),3);
				ev[4] = pow(*(theta+3),2);
				ev[5] = pow(*(theta+4),2);
				ev[6] = ev[4] * ev[5];
				ev[7] = pow(*(theta+5),2);
				ev[9] = *(theta+8) * *(theta+4);
				ev[10] = ev[9] * *(theta+5);
				ev[11] = ev[4] * ev[10];
				ev[15] = *(theta+6) * *(theta+4) * *(theta+3);
				ev[16] = ev[15] * ev[15];
				ev[20] = *(theta+7) * *(theta+5) * *(theta+3);
				ev[21] = ev[15] * ev[20];
				ev[24] = ev[20] * ev[15];
				ev[27] = ev[20] * ev[5];
				ev[29] = ev[6] * ev[7] - ev[11] * ev[10] - ev[16] * ev[7] + ev[21] * ev[10] + ev[24] * ev[10] - ev[27] * ev[20];
				ev[31] = ev[3] * sqrt(ev[29]);
				ev[33] = *(theta+9) * (1/ev[31]);
				ev[35] = x - *(theta+0);
				ev[40] = y - *(theta+1);
				ev[46] = z - *(theta+2);
				ev[47] = ev[15] * ev[10];
				ev[52] = ev[35] * (ev[35] * (ev[5] * ev[7] - ev[10] * ev[10]) + ev[40] * (ev[10] * ev[20] - ev[15] * ev[7]) + ev[46] * (ev[47] - ev[5] * ev[20]));
				ev[66] = ev[40] * (ev[35] * (ev[20] * ev[10] - ev[7] * ev[15]) + ev[40] * (ev[4] * ev[7] - ev[20] * ev[20]) + ev[46] * (ev[21] - ev[11]));
				ev[78] = ev[46] * (ev[35] * (ev[47] - ev[27]) + ev[40] * (ev[24] - ev[10] * ev[4]) + ev[46] * (ev[6] - ev[16]));
				ev[82] = exp(-0.5 * (ev[52]/ev[29] + ev[66]/ev[29] + ev[78]/ev[29]));
				ev[84] = 2 * *(theta+5);
				ev[92] = *(theta+7) * *(theta+3);
				ev[99] = ev[15] * ev[9];
				ev[107] = ev[4] * ev[9];
				ev[114] = ev[15] * ev[92];
				ev[119] = ev[92] * ev[15];
				ev[124] = ev[92] * ev[5];
				ev[128] = ev[6] * ev[84] - (ev[107] * ev[10] + ev[11] * ev[9]) - ev[16] * ev[84] + (ev[114] * ev[10] + ev[21] * ev[9]) + (ev[119] * ev[10] + ev[24] * ev[9]) - (ev[124] * ev[20] + ev[27] * ev[92]);
				ev[130] = pow(ev[29],2);

				*(grad+p)= -(ev[33] * (ev[82] * (0.5 * (ev[35] * (ev[35] * (ev[5] * ev[84] - (ev[9] * ev[10] + ev[10] * ev[9])) + ev[40] * (ev[9] * ev[20] + ev[10] * ev[92] - ev[15] * ev[84]) + ev[46] * (ev[99] - ev[5] * ev[92]))/ev[29] - ev[52] * ev[128]/ev[130] + (ev[40] * (ev[35] * (ev[92] * ev[10] + ev[20] * ev[9] - ev[84] * ev[15]) + ev[40] * (ev[4] * ev[84] - (ev[92] * ev[20] + ev[20] * ev[92])) + ev[46] * (ev[114] - ev[107]))/ev[29] - ev[66] * ev[128]/ev[130]) + (ev[46] * (ev[35] * (ev[99] - ev[124]) + ev[40] * (ev[119] - ev[9] * ev[4]))/ev[29] - ev[78] * ev[128]/ev[130])))) + *(theta+9) * (ev[3] * (0.5 * (ev[128] * (1/sqrt(ev[29]))))/pow(ev[31],2)) * ev[82]);

				p++;
			}
		}
	}
}

void dftheta6(double *theta, int *dimx, int *dimy, int *dimz, double *grad) {

    double ev[199];
    int x,y,z,p;

    p=0;
	for(z=1;z<(*dimz+1);z++) {
		for(y=1;y<(*dimy+1);y++) {
			for(x=1;x<(*dimx+1);x++) {

				ev[3] = pow(sqrt(2 * M_PI),3);
				ev[4] = pow(*(theta+3),2);
				ev[5] = pow(*(theta+4),2);
				ev[6] = ev[4] * ev[5];
				ev[7] = pow(*(theta+5),2);
				ev[10] = *(theta+8) * *(theta+4) * *(theta+5);
				ev[11] = ev[4] * ev[10];
				ev[15] = *(theta+6) * *(theta+4) * *(theta+3);
				ev[16] = ev[15] * ev[15];
				ev[20] = *(theta+7) * *(theta+5) * *(theta+3);
				ev[21] = ev[15] * ev[20];
				ev[24] = ev[20] * ev[15];
				ev[27] = ev[20] * ev[5];
				ev[29] = ev[6] * ev[7] - ev[11] * ev[10] - ev[16] * ev[7] + ev[21] * ev[10] + ev[24] * ev[10] - ev[27] * ev[20];
				ev[31] = ev[3] * sqrt(ev[29]);
				ev[33] = *(theta+9) * (1/ev[31]);
				ev[35] = x - *(theta+0);
				ev[40] = y - *(theta+1);
				ev[46] = z - *(theta+2);
				ev[47] = ev[15] * ev[10];
				ev[52] = ev[35] * (ev[35] * (ev[5] * ev[7] - ev[10] * ev[10]) + ev[40] * (ev[10] * ev[20] - ev[15] * ev[7]) + ev[46] * (ev[47] - ev[5] * ev[20]));
				ev[66] = ev[40] * (ev[35] * (ev[20] * ev[10] - ev[7] * ev[15]) + ev[40] * (ev[4] * ev[7] - ev[20] * ev[20]) + ev[46] * (ev[21] - ev[11]));
				ev[78] = ev[46] * (ev[35] * (ev[47] - ev[27]) + ev[40] * (ev[24] - ev[10] * ev[4]) + ev[46] * (ev[6] - ev[16]));
				ev[82] = exp(-0.5 * (ev[52]/ev[29] + ev[66]/ev[29] + ev[78]/ev[29]));
				ev[84] = *(theta+4) * *(theta+3);
				ev[85] = ev[84] * ev[10];
				ev[92] = ev[84] * ev[20];
				ev[96] = ev[84] * ev[15] + ev[15] * ev[84];
				ev[99] = ev[20] * ev[84];
				ev[101] = ev[92] * ev[10] - ev[96] * ev[7] + ev[99] * ev[10];
				ev[103] = pow(ev[29],2);

				*(grad+p) = -(ev[33] * (ev[82] * (0.5 * (ev[35] * (ev[46] * ev[85] - ev[40] * (ev[84] * ev[7]))/ev[29] - ev[52] * ev[101]/ev[103] + (ev[40] * (ev[46] * ev[92] - ev[35] * (ev[7] * ev[84]))/ev[29] - ev[66] * ev[101]/ev[103]) + (ev[46] * (ev[35] * ev[85] + ev[40] * ev[99] - ev[46] * ev[96])/ev[29] - ev[78] * ev[101]/ev[103])))) + *(theta+9) * (ev[3] * (0.5 * (ev[101] * (1/sqrt(ev[29]))))/pow(ev[31],2)) * ev[82]);

				p++;
			}
		}
	}
}

void dftheta7(double *theta, int *dimx, int *dimy, int *dimz, double *grad) {

    double ev[199];
    int x,y,z,p;

    p=0;
	for(z=1;z<(*dimz+1);z++) {
		for(y=1;y<(*dimy+1);y++) {
			for(x=1;x<(*dimx+1);x++) {

				ev[3] = pow(sqrt(2 * M_PI),3);
				ev[4] = pow(*(theta+3),2);
				ev[5] = pow(*(theta+4),2);
				ev[6] = ev[4] * ev[5];
				ev[7] = pow(*(theta+5),2);
				ev[10] = *(theta+8) * *(theta+4) * *(theta+5);
				ev[11] = ev[4] * ev[10];
				ev[15] = *(theta+6) * *(theta+4) * *(theta+3);
				ev[16] = ev[15] * ev[15];
				ev[20] = *(theta+7) * *(theta+5) * *(theta+3);
				ev[21] = ev[15] * ev[20];
				ev[24] = ev[20] * ev[15];
				ev[27] = ev[20] * ev[5];
				ev[29] = ev[6] * ev[7] - ev[11] * ev[10] - ev[16] * ev[7] + ev[21] * ev[10] + ev[24] * ev[10] - ev[27] * ev[20];
				ev[31] = ev[3] * sqrt(ev[29]);
				ev[33] = *(theta+9) * (1/ev[31]);
				ev[35] = x - *(theta+0);
				ev[40] = y - *(theta+1);
				ev[46] = z - *(theta+2);
				ev[47] = ev[15] * ev[10];
				ev[52] = ev[35] * (ev[35] * (ev[5] * ev[7] - ev[10] * ev[10]) + ev[40] * (ev[10] * ev[20] - ev[15] * ev[7]) + ev[46] * (ev[47] - ev[5] * ev[20]));
				ev[66] = ev[40] * (ev[35] * (ev[20] * ev[10] - ev[7] * ev[15]) + ev[40] * (ev[4] * ev[7] - ev[20] * ev[20]) + ev[46] * (ev[21] - ev[11]));
				ev[78] = ev[46] * (ev[35] * (ev[47] - ev[27]) + ev[40] * (ev[24] - ev[10] * ev[4]) + ev[46] * (ev[6] - ev[16]));
				ev[82] = exp(-0.5 * (ev[52]/ev[29] + ev[66]/ev[29] + ev[78]/ev[29]));
				ev[84] = *(theta+5) * *(theta+3);
				ev[92] = ev[15] * ev[84];
				ev[94] = ev[84] * ev[15];
				ev[97] = ev[84] * ev[5];
				ev[101] = ev[92] * ev[10] + ev[94] * ev[10] - (ev[97] * ev[20] + ev[27] * ev[84]);
				ev[103] = pow(ev[29],2);

				*(grad+p)= -(ev[33] * (ev[82] * (0.5 * (ev[35] * (ev[40] * (ev[10] * ev[84]) - ev[46] * (ev[5] * ev[84]))/ev[29] - ev[52] * ev[101]/ev[103] + (ev[40] * (ev[35] * (ev[84] * ev[10]) - ev[40] * (ev[84] * ev[20] + ev[20] * ev[84]) + ev[46] * ev[92])/ev[29] - ev[66] * ev[101]/ev[103]) + (ev[46] * (ev[40] * ev[94] - ev[35] * ev[97])/ev[29] - ev[78] * ev[101]/ev[103])))) + *(theta+9) * (ev[3] * (0.5 * (ev[101] * (1/sqrt(ev[29]))))/pow(ev[31],2)) * ev[82]);

				p++;
			}
		}
	}
}

void dftheta8(double *theta, int *dimx, int *dimy, int *dimz, double *grad) {

    double ev[199];
    int x,y,z,p;

    p=0;
	for(z=1;z<(*dimz+1);z++) {
		for(y=1;y<(*dimy+1);y++) {
			for(x=1;x<(*dimx+1);x++) {

				ev[3] = pow(sqrt(2 * M_PI),3);
				ev[4] = pow(*(theta+3),2);
				ev[5] = pow(*(theta+4),2);
				ev[6] = ev[4] * ev[5];
				ev[7] = pow(*(theta+5),2);
				ev[10] = *(theta+8) * *(theta+4) * *(theta+5);
				ev[11] = ev[4] * ev[10];
				ev[15] = *(theta+6) * *(theta+4) * *(theta+3);
				ev[16] = ev[15] * ev[15];
				ev[20] = *(theta+7) * *(theta+5) * *(theta+3);
				ev[21] = ev[15] * ev[20];
				ev[24] = ev[20] * ev[15];
				ev[27] = ev[20] * ev[5];
				ev[29] = ev[6] * ev[7] - ev[11] * ev[10] - ev[16] * ev[7] + ev[21] * ev[10] + ev[24] * ev[10] - ev[27] * ev[20];
				ev[31] = ev[3] * sqrt(ev[29]);
				ev[33] = *(theta+9) * (1/ev[31]);
				ev[35] = x - *(theta+0);
				ev[40] = y - *(theta+1);
				ev[46] = z - *(theta+2);
				ev[47] = ev[15] * ev[10];
				ev[52] = ev[35] * (ev[35] * (ev[5] * ev[7] - ev[10] * ev[10]) + ev[40] * (ev[10] * ev[20] - ev[15] * ev[7]) + ev[46] * (ev[47] - ev[5] * ev[20]));
				ev[66] = ev[40] * (ev[35] * (ev[20] * ev[10] - ev[7] * ev[15]) + ev[40] * (ev[4] * ev[7] - ev[20] * ev[20]) + ev[46] * (ev[21] - ev[11]));
				ev[78] = ev[46] * (ev[35] * (ev[47] - ev[27]) + ev[40] * (ev[24] - ev[10] * ev[4]) + ev[46] * (ev[6] - ev[16]));
				ev[82] = exp(-0.5 * (ev[52]/ev[29] + ev[66]/ev[29] + ev[78]/ev[29]));
				ev[84] = *(theta+4) * *(theta+5);
				ev[92] = ev[15] * ev[84];
				ev[98] = ev[4] * ev[84];
				ev[104] = ev[21] * ev[84] - (ev[98] * ev[10] + ev[11] * ev[84]) + ev[24] * ev[84];
				ev[106] = pow(ev[29],2);

				*(grad+p) = -(ev[33] * (ev[82] * (0.5 * (ev[35] * (ev[40] * (ev[84] * ev[20]) - ev[35] * (ev[84] * ev[10] + ev[10] * ev[84]) + ev[46] * ev[92])/ev[29] - ev[52] * ev[104]/ev[106] + (ev[40] * (ev[35] * (ev[20] * ev[84]) - ev[46] * ev[98])/ev[29] - ev[66] * ev[104]/ev[106]) + (ev[46] * (ev[35] * ev[92] - ev[40] * (ev[84] * ev[4]))/ev[29] - ev[78] * ev[104]/ev[106])))) + *(theta+9) * (ev[3] * (0.5 * (ev[104] * (1/sqrt(ev[29]))))/pow(ev[31],2)) * ev[82]);

				p++;
			}
		}
	}
}

void dftheta9(double *theta, int *dimx, int *dimy, int *dimz, double *grad) {

    double ev[199];
    int x,y,z,p;

    p=0;
	for(z=1;z<(*dimz+1);z++) {
		for(y=1;y<(*dimy+1);y++) {
			for(x=1;x<(*dimx+1);x++) {

				ev[4] = pow(*(theta+3),2);
				ev[5] = pow(*(theta+4),2);
				ev[6] = ev[4] * ev[5];
				ev[7] = pow(*(theta+5),2);
				ev[10] = *(theta+8) * *(theta+4) * *(theta+5);
				ev[11] = ev[4] * ev[10];
				ev[15] = *(theta+6) * *(theta+4) * *(theta+3);
				ev[16] = ev[15] * ev[15];
				ev[20] = *(theta+7) * *(theta+5) * *(theta+3);
				ev[21] = ev[15] * ev[20];
				ev[24] = ev[20] * ev[15];
				ev[27] = ev[20] * ev[5];
				ev[29] = ev[6] * ev[7] - ev[11] * ev[10] - ev[16] * ev[7] + ev[21] * ev[10] + ev[24] * ev[10] - ev[27] * ev[20];
				ev[32] = 1/(pow(sqrt(2 * M_PI),3) * sqrt(ev[29]));
				ev[35] = x - *(theta+0);
				ev[40] = y - *(theta+1);
				ev[46] = z - *(theta+2);
				ev[47] = ev[15] * ev[10];
				ev[82] = exp(-0.5 * (ev[35] * (ev[35] * (ev[5] * ev[7] - ev[10] * ev[10]) + ev[40] * (ev[10] * ev[20] - ev[15] * ev[7]) + ev[46] * (ev[47] - ev[5] * ev[20]))/ev[29] + ev[40] * (ev[35] * (ev[20] * ev[10] - ev[7] * ev[15]) + ev[40] * (ev[4] * ev[7] - ev[20] * ev[20]) + ev[46] * (ev[21] - ev[11]))/ev[29] + ev[46] * (ev[35] * (ev[47] - ev[27]) + ev[40] * (ev[24] - ev[10] * ev[4]) + ev[46] * (ev[6] - ev[16]))/ev[29]));

				*(grad+p) = ev[32] * ev[82];

				p++;
			}
		}
	}
}


void gaussFix(double *theta, double *fixed, int *np, int *dimx, int *dimy, int *dimz, double *gx)
{

	int reg,x,y,z,p;
	double f,theta_x,theta_y,theta_z,sig_x,sig_y,sig_z,sig_xy,sig_xz,sig_yz,det_sig,dif_x,dif_y,dif_z;

	//theta 1,2,3 = x,y,z coordinates
	//theta 4,5,6 = sd's of x,y,z
	//theta 7,8,9 = corr xy, xz, yz
	//theta 10    = amplitude

	p=0;
	for(z=1;z<(*dimz+1);z++) {
		for(y=1;y<(*dimy+1);y++) {
			for(x=1;x<(*dimx+1);x++) {

				f=0; //f becomes the sum of all regions  (zeroed every region)

				for(reg=0;reg<(*np);reg=reg+10) {

					if(fixed[reg+0]!=0e0) theta[reg+0]=fixed[reg+0];
					if(fixed[reg+1]!=0e0) theta[reg+1]=fixed[reg+1];
					if(fixed[reg+2]!=0e0) theta[reg+2]=fixed[reg+2];
					if(fixed[reg+3]!=0e0) theta[reg+3]=fixed[reg+3];
					if(fixed[reg+4]!=0e0) theta[reg+4]=fixed[reg+4];
					if(fixed[reg+5]!=0e0) theta[reg+5]=fixed[reg+5];
					if(fixed[reg+6]!=0e0) theta[reg+6]=fixed[reg+6];
					if(fixed[reg+7]!=0e0) theta[reg+7]=fixed[reg+7];
					if(fixed[reg+8]!=0e0) theta[reg+8]=fixed[reg+8];
					if(fixed[reg+9]!=0e0) theta[reg+9]=fixed[reg+9];


					//parameter coordinates
					theta_x=theta[reg+0];
					theta_y=theta[reg+1];
					theta_z=theta[reg+2];

					//sigma matrix
					sig_x=pow(theta[reg+3],2);
					sig_xy=theta[reg+6]*theta[reg+4]*theta[reg+3];
					sig_xz=theta[reg+7]*theta[reg+5]*theta[reg+3];
					sig_y=pow(theta[reg+4],2);
					sig_yz=theta[reg+8]*theta[reg+4]*theta[reg+5];
					sig_z=pow(theta[reg+5],2);


					//determinant of sigma
					det_sig=sig_x*sig_y*sig_z-sig_x*sig_yz*sig_yz-sig_xy*sig_xy*sig_z+sig_xy*sig_xz*sig_yz+sig_xz*sig_xy*sig_yz-sig_xz*sig_y*sig_xz;
					if(det_sig < 0) det_sig=0;

					//(x-pc)
					dif_x=(x-theta_x);
					dif_y=(y-theta_y);
					dif_z=(z-theta_z);

					//add to f gaussian value for each region
					f=f+theta[reg+9]*(1/(pow(sqrt(2*M_PI),3)*sqrt(det_sig)))*exp(-.5*(dif_x*(dif_x*(sig_y*sig_z-sig_yz*sig_yz)+dif_y*(sig_yz*sig_xz-sig_xy*sig_z)+dif_z*(sig_xy*sig_yz-sig_y*sig_xz))/det_sig+dif_y*(dif_x*(sig_xz*sig_yz-sig_z*sig_xy)+dif_y*(sig_x*sig_z-sig_xz*sig_xz)+dif_z*(sig_xy*sig_xz-sig_x*sig_yz))/det_sig+dif_z*(dif_x*(sig_xy*sig_yz-sig_xz*sig_y)+dif_y*(sig_xz*sig_xy-sig_yz*sig_x)+dif_z*(sig_x*sig_y-sig_xy*sig_xy))/det_sig));

				}

				gx[p]=f; //set output vector to sum of gaussian
				p++;

			}
		}
	}
}

void ssqgaussFix(double *theta, double *fixed, double *dat, double *W, int *np, int *dimx, int *dimy, int *dimz, double *ss)
{

	int reg,x,y,z,p;
	double f,g,theta_x,theta_y,theta_z,sig_x,sig_y,sig_z,sig_xy,sig_xz,sig_yz,det_sig,dif_x,dif_y,dif_z;

	//theta 1,2,3 = x,y,z coordinates
	//theta 4,5,6 = sd's of x,y,z
	//theta 7,8,9 = corr xy, xz, yz
	//theta 10    = amplitude

	p=0;
	g=0e0;
	for(z=1;z<(*dimz+1);z++) {
		for(y=1;y<(*dimy+1);y++) {
			for(x=1;x<(*dimx+1);x++) {

				f=0; //f becomes the sum of all regions  (zeroed every region)

				for(reg=0;reg<(*np);reg=reg+10) {

					if(fixed[reg+0]!=0e0) theta[reg+0]=fixed[reg+0];
					if(fixed[reg+1]!=0e0) theta[reg+1]=fixed[reg+1];
					if(fixed[reg+2]!=0e0) theta[reg+2]=fixed[reg+2];
					if(fixed[reg+3]!=0e0) theta[reg+3]=fixed[reg+3];
					if(fixed[reg+4]!=0e0) theta[reg+4]=fixed[reg+4];
					if(fixed[reg+5]!=0e0) theta[reg+5]=fixed[reg+5];
					if(fixed[reg+6]!=0e0) theta[reg+6]=fixed[reg+6];
					if(fixed[reg+7]!=0e0) theta[reg+7]=fixed[reg+7];
					if(fixed[reg+8]!=0e0) theta[reg+8]=fixed[reg+8];
					if(fixed[reg+9]!=0e0) theta[reg+9]=fixed[reg+9];


					//parameter coordinates
					theta_x=theta[reg+0];
					theta_y=theta[reg+1];
					theta_z=theta[reg+2];

					//sigma matrix
					sig_x=pow(theta[reg+3],2);
					sig_xy=theta[reg+6]*theta[reg+4]*theta[reg+3];
					sig_xz=theta[reg+7]*theta[reg+5]*theta[reg+3];
					sig_y=pow(theta[reg+4],2);
					sig_yz=theta[reg+8]*theta[reg+4]*theta[reg+5];
					sig_z=pow(theta[reg+5],2);


					//determinant of sigma
					det_sig=sig_x*sig_y*sig_z-sig_x*sig_yz*sig_yz-sig_xy*sig_xy*sig_z+sig_xy*sig_xz*sig_yz+sig_xz*sig_xy*sig_yz-sig_xz*sig_y*sig_xz;
					if(det_sig < 0) det_sig=0;

					//(x-pc)
					dif_x=(x-theta_x);
					dif_y=(y-theta_y);
					dif_z=(z-theta_z);

					//add to f gaussian value for each region
					f=f+theta[reg+9]*(1/(pow(sqrt(2*M_PI),3)*sqrt(det_sig)))*exp(-.5*(dif_x*(dif_x*(sig_y*sig_z-sig_yz*sig_yz)+dif_y*(sig_yz*sig_xz-sig_xy*sig_z)+dif_z*(sig_xy*sig_yz-sig_y*sig_xz))/det_sig+dif_y*(dif_x*(sig_xz*sig_yz-sig_z*sig_xy)+dif_y*(sig_x*sig_z-sig_xz*sig_xz)+dif_z*(sig_xy*sig_xz-sig_x*sig_yz))/det_sig+dif_z*(dif_x*(sig_xy*sig_yz-sig_xz*sig_y)+dif_y*(sig_xz*sig_xy-sig_yz*sig_x)+dif_z*(sig_x*sig_y-sig_xy*sig_xy))/det_sig));

				}

				//sum (data-model)^2 over voxels and weight
				g=g+pow((dat[p]-f),2)*(1/W[p]);
				p++;

			}
		}
	}

	//set ss to g
	ss[0]=g;
}